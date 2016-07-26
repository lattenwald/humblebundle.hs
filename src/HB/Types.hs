{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-#LANGUAGE PartialTypeSignatures #-}
module HB.Types where

import           Control.Applicative
import           Control.Monad
import           Crypto.Hash
import           Data.Aeson
import qualified Data.ByteString.Base16 as B16
import qualified Data.HashMap.Strict as HashMap
import           Data.Text.Encoding (encodeUtf8)

import qualified Data.Map as Map
import qualified Data.Text as T

data Platform = Windows | Mac | Linux | Android | Audio | Ebook | Asmjs
     deriving (Show, Read, Eq)
instance FromJSON Platform where
  parseJSON = withText "platform" $ \t ->
    case t of
      "windows" -> pure Windows
      "mac"     -> pure Mac
      "linux"   -> pure Linux
      "android" -> pure Android
      "audio"   -> pure Audio
      "ebook"   -> pure Ebook
      "asmjs"   -> pure Asmjs
      _         -> fail "unknown platform"

data DLType = DLT String | DLTPatch | DLTInstructions | DLTAudio String | DLTEbook String | DLTDebug String
  deriving (Show, Eq)
instance FromJSON DLType where
  parseJSON = withText "type" $ \t ->
    case t of
      "Instructions"      -> pure DLTInstructions

      s | "Download Tablet" `T.isPrefixOf` s
          -> pure $ DLT "tablet"
      "Download"                 -> pure $ DLT ""
      "Intel Only"               -> pure $ DLT "intel"
      "Tablet"                   -> pure $ DLT "tablet"
      "Phone"                    -> pure $ DLT "phone"
      "Patch"                    -> pure $ DLT "patch"
      "Download Mobile"          -> pure $ DLT "mobile"
      "Download Classic"         -> pure $ DLT "classic"
      "Download Extended"        -> pure $ DLT "extended"
      "Download (HD)"            -> pure $ DLT "hd"
      "Download (Regular)"       -> pure $ DLT ""
      "Download Cogs"            -> pure $ DLT ""
      "Cogs GO (Netbooks)"       -> pure $ DLT "netbook"
      "New"                      -> pure $ DLT "new"
      "Alternate"                -> pure $ DLT "alt"
      "Recommended"              -> pure $ DLT ""
      "Mobile"                   -> pure $ DLT "mobile"
      s | "Updated Mobile " `T.isPrefixOf` s
          -> pure $ DLT "mobile"
      s | "HD " `T.isPrefixOf` s
       || "Updated HD " `T.isPrefixOf` s
          -> pure $ DLT "hd"
      "Original (OS 1.6+)"       -> pure $ DLT "old"
      "Download DLC"             -> pure $ DLT "dlc"
      "Older Version"            -> pure $ DLT "old"
      "Updated Version"          -> pure $ DLT ""

      "atitc"                 -> pure $ DLT "ATITC"
      "etc"                   -> pure $ DLT "ETC"
      "dxt"                   -> pure $ DLT "DXT"
      "axt"                   -> pure $ DLT "Tegra"
      "avr"                   -> pure $ DLT "PowerVR"
      "atc"                   -> pure $ DLT "Adreno"
      "ATC"                   -> pure $ DLT "Adreno"
      "DXT"                   -> pure $ DLT "DXT"
      "PVRTC"                 -> pure $ DLT "PVRTC"
      "Samsung ATC"           -> pure $ DLT "Adreno"
      "ATITC (Android 5.1.1)" -> pure $ DLT "ATITC"
      "ETC"                   -> pure $ DLT "ETC"
      "S3TC"                  -> pure $ DLT "S3TC"
      "ETC1"                  -> pure $ DLT "ETC1"
      "ETC2"                  -> pure $ DLT "ETC2"
      "PVRTC1"                -> pure $ DLT "PVRTC1"
      "PVRTC2"                -> pure $ DLT "PVRTC2"
      "OBB"                   -> pure $ DLT "obb"
      "APK"                   -> pure $ DLT "apk"
      "ARMv7 APK"             -> pure $ DLT "arm7"
      "x86 APK"               -> pure $ DLT "x86"

      "MP3"  -> pure $ DLTAudio "mp3"
      "FLAC" -> pure $ DLTAudio "flac"
      "OGG"  -> pure $ DLTAudio "ogg"
      "WAV"  -> pure $ DLTAudio "wav"
      s | " (MP3)" `T.isSuffixOf` s
          -> pure $ DLTAudio "mp3"

      "PDF" -> pure $ DLTEbook "PDF"
      "PDF (HQ)" -> pure $ DLTEbook "PDF"
      "EPUB" -> pure $ DLTEbook "EPUB"
      "MOBI" -> pure $ DLTEbook "MOBI"

      "Deutsch"     -> pure $ DLT ""
      "Fran\231ais" -> pure $ DLT ""
      "English"     -> pure $ DLT ""
      "Slovak"      -> pure $ DLT ""
      "German"      -> pure $ DLT ""


      "Native .bin"      -> pure $ DLT ""
      ".tar.bz2"         -> pure $ DLT ""
      ".bin"             -> pure $ DLT ""
      ".tgz"             -> pure $ DLT ""
      ".tar.gz"          -> pure $ DLT ""
      "tar.gz"           -> pure $ DLT ""
      ".zip"             -> pure $ DLT ""
      "32-bit .zip"      -> pure $ DLT ""
      "64-bit .zip"      -> pure $ DLT ""
      ".deb"             -> pure $ DLT ""
      ".rpm"             -> pure $ DLT ""
      ".run"             -> pure $ DLT ""
      ".sh"              -> pure $ DLT ""
      ".tar"             -> pure $ DLT ""
      "32-bit .deb"      -> pure $ DLT ""
      "64-bit .deb"      -> pure $ DLT ""
      "32-bit .tar.gz"   -> pure $ DLT ""
      "64-bit .tar.gz"   -> pure $ DLT ""
      "32-bit tar.gz"    -> pure $ DLT ""
      "64-bit tar.gz"    -> pure $ DLT ""
      "64-bit .rpm"      -> pure $ DLT ""
      "32-bit .rpm"      -> pure $ DLT ""
      "i386.deb"         -> pure $ DLT ""
      "x86_64.deb"       -> pure $ DLT ""
      "i386.tar.gz"      -> pure $ DLT ""
      "x86_64.tar.gz"    -> pure $ DLT ""
      "32-bit"           -> pure $ DLT ""
      "64-bit"           -> pure $ DLT ""
      "150 DPI"          -> pure $ DLT "150dpi"
      "300 DPI"          -> pure $ DLT "300dpi"
      "Download .tar.gz" -> pure $ DLT ""
      "Download .rpm"    -> pure $ DLT ""
      "Download .deb"    -> pure $ DLT ""
      "Download App"     -> pure $ DLT ""
      "Download DMG"     -> pure $ DLT ""

      "Flash"          -> pure $ DLT "flash"
      "Flash Package"  -> pure $ DLT "flash"
      "Air"            -> pure $ DLT "air"
      "AIR"            -> pure $ DLT "air"
      "Download Air"   -> pure $ DLT "air"
      "Mojo Installer" -> pure $ DLT "mojo"
      "Web Installer"  -> pure $ DLT "web"
      ".mojo.run"      -> pure $ DLT "mojo"
      "Installer"      -> pure $ DLT ""

      other -> fail $ "Unknown download type: " ++ show other

strToDigest = undefined

instance FromJSON (Digest MD5) where
  parseJSON = withText "md5 digest" $ \t ->
    case digestFromByteString . fst . B16.decode . encodeUtf8 $ t of
      Just d -> pure d
      Nothing -> fail "not an md5 digest"

data DL = DL { hname       :: String
             , mname       :: String
             , bundle_name :: String
             , platform    :: Platform
             , dltype      :: Maybe DLType
             , url         :: String
             , hsize       :: Maybe String
             , fsize       :: Maybe Int
             , md5         :: Maybe (Digest MD5)
             } deriving (Show, Eq)
instance Ord DL where
  dl1 `compare` dl2 = (fsize dl1) `compare` (fsize dl2)
instance FromJSON [DL] where
  parseJSON = withObject "bundle" $ \o -> do
    bundle_name <- o .: "product" >>= (.: "human_name")
    sps         <- o .: "subproducts"
    fmap join $ forM sps $ \sp -> do
      hname <- sp .: "human_name"
      dls   <- sp .: "downloads"
      stuff <- forM (filter ("platform" `HashMap.member`) dls) $ \dl -> do
        mname     <- dl .: "machine_name"
        platform  <- dl .: "platform"
        dlstructs <- dl .: "download_struct"
        forM (filter ("url" `HashMap.member`) dlstructs) $ \dlstruct -> do
          md5    <-  dlstruct .:? "md5"
          dltype <- dlstruct .:? "name"
          url    <- (dlstruct .: "url") <|> (dlstruct .: "url" >>= (.: "web")) <|> (fail $ "url not found for bundle " ++ show bundle_name ++ " at " ++ show mname ++ " with md5 " ++ show md5)
          hsize  <-  dlstruct .:? "human_size"
          fsize  <-  dlstruct .:? "file_size"
          return DL {..}
      pure $ join stuff

data Platform' = Platform' Platform | All
  deriving (Show, Eq)

strToPlatform' s = case s of
  "All" -> All
  a -> Platform' (read a)

newtype FileBaseName = FileBaseName { unFileBaseName :: FilePath }
  deriving (Show, Eq, Ord)
newtype FileRelName  = FileRelName  { unFileRelName  :: FilePath }
  deriving (Show, Eq, Ord)
newtype FileAbsName  = FileAbsName  { unFileAbsName  :: FilePath }
  deriving (Show, Eq, Ord)
newtype DirRelName   = DirRelName   { unDirRelName   :: FilePath }
  deriving (Show, Eq, Ord)
newtype DirAbsName   = DirAbsName   { unDirAbsName   :: FilePath }
  deriving (Show, Eq, Ord)

newtype Hashes = Hashes { getHashes:: Map.Map FileRelName (Maybe (Digest MD5)) }
  deriving (Show, Eq)
