{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
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

import qualified Debug.Trace as D

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

data DLType = DLTDownload | DLTTablet | DLTPatch | DLTIntelOnly
  deriving (Show, Eq)
instance FromJSON DLType where
  parseJSON = withText "type" $ \t ->
    case t of
      "Intel Only"                           -> pure DLTIntelOnly
      "Patch"                                -> pure DLTPatch
      s | "Download Tablet" `T.isPrefixOf` s -> pure DLTTablet
      "Download"                             -> pure DLTDownload
      _                                      -> fail "unknown download type"

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
    forM (filter ("platform" `HashMap.member`) sps) $ \sp -> D.traceShowId <$> do
      hname    <- sp .: "human_name"
      mname    <- sp .: "machine_name"
      url      <- D.traceShowId <$> (sp .: "url") <|> (sp .: "url" >>= (.: "web"))
      hsize    <- sp .:? "human_size"
      fsize    <- sp .:? "file_size"
      md5      <- sp .:? "md5"
      platform <- sp .: "platform"
      dltype   <- sp .:? "type"
      pure DL {..}

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
