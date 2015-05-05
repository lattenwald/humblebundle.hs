{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module HB.Utils where

import Data.List
import Data.Aeson.Lens
import Control.Lens
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
import Data.Text.Lens

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

fetch :: Manager -> CookieJar -> String -> IO BL8.ByteString
fetch m jar key = do
  req <- parseUrl ("https://www.humblebundle.com/api/v1/order/" ++ key)
  let req' = req { cookieJar = Just jar }
  responseBody <$> httpLbs req' m

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
  let hname' = map (\c -> if c == ':' then '_' else c) hname
      fdir  = concat [dir, "/", show platform, "/"
                     , hname'
                     , if dltype == Just DLTTablet then "/tablet" else ""
                     ]
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
        putStrLn $ bundle_name ++ ", " ++ hname ++ " (" ++ mname ++ ")" ++ " md5: got "  ++ show md5'  ++ ", expected " ++ show md5
  when (isJust sha1_ok && not (fromJust sha1_ok) && isJust sha1') $
        putStrLn $ bundle_name ++ ", " ++ hname ++ " (" ++ mname ++ ")" ++ " sha1: got "  ++ show sha1'  ++ ", expected " ++ show sha1
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

parseBundle :: BL8.ByteString -> [DL]
parseBundle str = do
  let Just json = decode str :: Maybe Value
  bundle_name <- json ^.. key "product" . key "human_name" . _String . from packed

  sp <- json ^.. key "subproducts" . values
  human_name <- sp ^.. key "human_name" . _String . from packed

  dl <- sp ^.. key "downloads" . values
  machine_name <- dl ^.. key "machine_name" . _String . from packed
  platform <- dl ^.. key "platform" . _Platform

  dl_struct <- dl ^.. key "download_struct" . values
  url <- dl_struct ^.. key "url" . key "web" . _String . from packed
  let dl_type = dl_struct ^? key "name" . _DLType
      human_size = dl_struct ^? key "human_size" . _String . from packed
      file_size = dl_struct ^? key "file_size" . _Integral
      sha1 = dl_struct ^? key "sha1" . _Digest
      md5 = dl_struct ^? key "md5" . _Digest

  return $ DL human_name machine_name bundle_name platform dl_type url human_size file_size sha1 md5

filterPlatform All = id
filterPlatform (Platform' pl) = filter ((pl ==) . platform)
