{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module HB.Utils where

import           Control.Lens
import           Control.Monad
import           Crypto.Hash
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.Binary as Bin
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Text.Encoding (encodeUtf8)
import           Data.Text.Lens
import           HB.Types
import           Network.HTTP.Client.Utils
import           Pipes
import qualified Pipes.ByteString as PB
import           Pipes.HTTP
import qualified Pipes.Prelude as P
import           System.Directory
import           System.FilePath
import           System.IO

fetch :: Manager -> CookieJar -> String -> IO BL8.ByteString
fetch m jar key = do
  req <- parseUrl ("https://www.humblebundle.com/api/v1/order/" ++ key)
  let req' = req { cookieJar = Just jar }
  responseBody <$> httpLbs req' m

download m url fname = do
  req <- parseUrl url
  withHTTP req m $ \resp ->
    withFile fname WriteMode $ \h ->
      runEffect $ responseBody resp >-> PB.toHandle h

executeDownload :: Manager -> Hashes -> DirAbsName -> Bool -> DL
                   -> IO (FileRelName, Maybe (Digest MD5))
executeDownload m hashes dir verbose dl@DL{..} = do
  req <- parseUrl url
  let hname'   = map (\c -> if c `elem` (":*/<>?|" :: String) then '_' else c) hname
      reldir   = DirRelName $ concat [show platform, "/"
                                     , hname'
                                     , if dltype == Just DLTTablet then "/tablet" else ""
                                     ]
      filedir  = DirAbsName $ concat [unDirAbsName dir, "/", unDirRelName reldir]
      fname    = FileBaseName $ takeFileName . B8.unpack . path $ req
      relname  = FileRelName $ concat [unDirRelName reldir, "/", unFileBaseName fname]
      fullname = FileAbsName $ concat [unDirAbsName dir, "/", unFileRelName relname]
  let fok = fileOK hashes dir relname verbose dl
  ok <- fok
  unless ok $ do
    createDirectoryIfMissing True $ unDirAbsName filedir
    let printName = hname ++ if isJust hsize
                             then " (" ++ fromJust hsize ++ ")"
                             else ""
    putStrLn $ "Downloading " ++ printName ++ "..."
    download m url $ unFileAbsName fullname
    ok2 <- fok
    unless ok2 $ fail $ "failed downloading file " ++ show fullname
    putStrLn $ "Done with " ++ printName
  return (relname, md5)

fileOK :: Hashes -> DirAbsName -> FileRelName -> Bool -> DL -> IO Bool
fileOK hashes dir relname verbose DL{..} = do
  let fullname = FileAbsName $ concat [unDirAbsName dir, "/", unFileRelName relname]
  md5' <- case join (Map.lookup relname . getHashes $ hashes) of
            h@(Just _) -> return h
            Nothing -> fileHash fullname
  let md5_ok = (==) <$> md5  <*> md5'
  when (verbose && isJust md5_ok && not (fromJust md5_ok) && isJust md5') $
        putStrLn $ bundle_name ++ ", " ++ hname ++ " (" ++ mname ++ ")" ++ " md5: got "  ++ show md5'  ++ ", expected " ++ show md5
  return $ isJust md5_ok && fromJust md5_ok

fileHash :: HashAlgorithm a => FileAbsName -> IO (Maybe (Digest a))
fileHash fname = do
  e <- doesFileExist $ unFileAbsName fname
  if e
     then Just <$> fhash
     else return Nothing
  where
    fhash = withFile (unFileAbsName fname) ReadMode $ \h ->
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
      md5 = dl_struct ^? key "md5" . _Digest

  return $ DL human_name machine_name bundle_name platform dl_type url human_size file_size md5

filterPlatform All = id
filterPlatform (Platform' pl) = filter ((pl ==) . platform)

strToDigest :: HashAlgorithm a => B8.ByteString -> Maybe (Digest a)
strToDigest = digestFromByteString . fst . B16.decode

_Digest :: HashAlgorithm a => Prism' Value (Digest a)
_Digest = prism' undefined strToDigest'
  where
    strToDigest' (String s) = strToDigest . encodeUtf8 $ s
    strToDigest' _ = Nothing

_Platform :: Prism' Value Platform
_Platform = prism' undefined strToPlatform
  where
    strToPlatform "windows" = Just Windows
    strToPlatform "mac"     = Just Mac
    strToPlatform "linux"   = Just Linux
    strToPlatform "android" = Just Android
    strToPlatform "audio"   = Just Audio
    strToPlatform "ebook"   = Just Ebook
    strToPlatform "asmjs"   = Just Asmjs
    strToPlatform _         = Nothing

saveHashes :: Hashes -> FileRelName -> IO ()
saveHashes h f = BL8.writeFile (unFileRelName f ++ ".bin") . Bin.encode $ h

loadHashes :: FileRelName -> IO Hashes
loadHashes f = BL8.readFile (unFileRelName f ++ ".bin") >>= return . Bin.decode

uniq :: Ord a => [a] -> [a]
uniq = Set.toList . Set.fromList
