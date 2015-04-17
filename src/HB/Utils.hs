{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module HB.Utils where

import Data.List
import System.IO
import Network.HTTP.Client.Utils
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import System.FilePath
import System.Directory
import Control.Monad
import Crypto.Hash
import qualified Pipes.Prelude as P

import Data.Aeson
import Pipes
import Pipes.HTTP
import qualified Pipes.ByteString as PB

import HB.Types

credentials :: IO (B.ByteString, B.ByteString)
credentials = do
  h <- openFile "credentials" ReadMode
  username <- B.hGetLine h
  password <- B.hGetLine h
  return (username, password)

auth m username password = do
  res <- parseUrl "https://www.humblebundle.com"
    >>= flip httpNoBody m
    >>= clickWith m (parseUrl "https://www.humblebundle.com/login/captcha") toPost
    >>= clickWith m (parseUrl "https://www.humblebundle.com/processlogin") (
        insertCsrf
      . addPostFields [("username", username), ("password", password)]
      . toPost )
  return res

fetch :: Manager -> CookieJar -> String -> IO (Either String Bundle)
fetch m jar key = do
  req <- parseUrl ("https://www.humblebundle.com/api/v1/order/" ++ key)
  let req' = req { cookieJar = Just jar }
  bundle <- eitherDecode . responseBody <$> httpLbs req' m
  return bundle

download m url fname = do
  req <- parseUrl url
  withHTTP req m $ \resp -> do
    withFile fname WriteMode $ \h ->
      runEffect $ responseBody resp >-> PB.toHandle h

getKeys :: BL8.ByteString -> Maybe [String]
getKeys body = k
  where
    searchString = "gamekeys = "
    gamekeysLocation = fromIntegral . (length searchString +) <$>
                       bsSearch searchString body
    gamekeys = BL8.takeWhile (/= ']') . flip BL8.drop body <$> gamekeysLocation
    k = read . (++ "]") . BL8.unpack <$> gamekeys


bsSearch :: String -> BL8.ByteString -> Maybe Int
bsSearch term str = findConsecutive indices
  where
    indices = map (map fromIntegral . flip BL8.elemIndices str) term
    findConsecutive [] = Nothing
    findConsecutive (xs:rest) = case sort . filter isJust . map (flip findConsecutive' rest) $ xs of
                                     [] -> Nothing
                                     (x:_) -> (subtract (length rest)) <$> x
    findConsecutive' i [] = Just i
    findConsecutive' _ ([]:_) = Nothing
    findConsecutive' i ((x:xs):rest) = case (i+1) `compare` x of
                                            EQ -> findConsecutive' x rest
                                            GT -> findConsecutive' i (xs:rest)
                                            LT -> Nothing


executeDownload :: Manager -> String -> DL -> IO ()
executeDownload m dir dl@DL{..} = do
  req <- parseUrl url
  let fdir  = concat [dir, "/", show platform, "/", hname]
      fname = takeFileName . B8.unpack . path $ req
      fullname = concat [fdir, "/", fname]
  createDirectoryIfMissing True fdir
  ok <- fileOK fullname dl
  when (not ok) $ do
    putStrLn $ hname ++ if isJust hsize
                        then " (" ++ fromJust hsize ++ ")"
                        else ""
    download m url fullname
    ok2 <- fileOK fullname dl
    when (not ok2) $ fail $ "failed downloading file " ++ show fullname

fileOK :: FilePath -> DL -> IO Bool
fileOK fullname DL{..} = do
  e <- doesFileExist fullname
  md5'  <- if e then Just <$> (fileHash fullname :: IO (Digest MD5 )) else return Nothing
  sha1' <- if e then Just <$> (fileHash fullname :: IO (Digest SHA1)) else return Nothing
  let md5_ok  = (==) <$> md5  <*> md5'
      sha1_ok = (==) <$> sha1 <*> sha1'
  when (isJust md5_ok && not (fromJust md5_ok) && isJust md5') $
        putStrLn $ "md5: got "  ++ show md5'  ++ ", expected " ++ show md5
  when (isJust sha1_ok && not (fromJust sha1_ok) && isJust sha1') $
        putStrLn $ "sha1: got "  ++ show sha1'  ++ ", expected " ++ show sha1
  return $ any fromJust . filter isJust $ [md5_ok, sha1_ok]

fileHash :: HashAlgorithm a => FilePath -> IO (Digest a)
fileHash fname = do
  withFile fname ReadMode $ \h -> do
    P.fold (\ctx -> hashUpdates ctx . pure)
      hashInit hashFinalize (PB.fromHandle h)

fromRight (Right a) = a
fromRight _ = error "Not Right"

isRight (Right _) = True
isRight _ = False

isJust (Just _) = True
isJust _ = False

fromJust (Just a) = a
fromJust _ = error "Not Just"

extractDLs :: Platform' -> [Bundle] -> [DL]
extractDLs p bundles = do
  bundle <- bundles
  sp <- subproducts bundle
  dl <- sp_downloads sp
  guard $ filterPlatform p dl
  ds <- dl_download_struct dl
  let u = dsu_web (ds_url ds)
      dlt = ds_type ds
  guard $ isJust u
  guard $ (not (isJust dlt) || (dlt == Just DLTDownload))
  return $ DL (sp_human_name sp)
              (dl_machine_name dl)
              (dl_platform dl)
              dlt
              (fromJust u)
              (ds_human_size ds)
              (ds_file_size ds)
              (ds_sha1 ds)
              (ds_md5 ds)
  where
    filterPlatform p dl = p == All || Platform' (dl_platform dl) == p

