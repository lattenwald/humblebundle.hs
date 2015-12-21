{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module HB.Utils where

import           Control.Monad
import           Crypto.Hash
import qualified Data.Binary as Bin
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Map as Map
import qualified Data.Set as Set
import           HB.Derive ()
import           HB.Types
import           Pipes
import qualified Pipes.ByteString as PB
import           Pipes.HTTP
import qualified Pipes.Prelude as P
import           System.Directory
import           System.FilePath
import           System.IO

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

filterPlatform All = id
filterPlatform (Platform' pl) = filter ((pl ==) . platform)

saveHashes :: Hashes -> FileRelName -> IO ()
saveHashes h f = BL8.writeFile (unFileRelName f ++ ".bin") . Bin.encode $ h

loadHashes :: FileRelName -> IO Hashes
loadHashes f = do
  exists <- doesFileExist (unFileRelName f)
  if exists
     then BL8.readFile (unFileRelName f ++ ".bin") >>= return . Bin.decode
     else pure $ Hashes Map.empty

uniq :: Ord a => [a] -> [a]
uniq = Set.toList . Set.fromList
