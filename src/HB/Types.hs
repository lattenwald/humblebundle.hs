{-# LANGUAGE OverloadedStrings #-}
module HB.Types where

import           Data.Aeson
import           Control.Lens
import           Crypto.Hash
import qualified Data.Text as T
import qualified Data.Map as Map

data Platform = Windows | Mac | Linux | Android | Audio | Ebook | Asmjs
     deriving (Show, Read, Eq)

data DLType = DLTDownload | DLTTablet | DLTPatch | DLTIntelOnly
  deriving (Show, Eq)

_DLType :: Prism' Value DLType
_DLType = prism' undefined strToDLType
  where
    strToDLType (String "Intel Only")      = Just DLTIntelOnly
    strToDLType (String "Patch")           = Just DLTPatch
    strToDLType (String s)
      | "Download Tablet" `T.isPrefixOf` s = Just DLTTablet
    strToDLType (String "Download")        = Just DLTDownload
    strToDLType _                          = Nothing

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

data Platform' = Platform' Platform | All
  deriving (Show, Eq)

strToPlatform' s = case s of
  "All" -> All
  a -> Platform' (read a)

data FileBaseName = FileBaseName { unFileBaseName :: FilePath } deriving (Show, Eq, Ord)
data FileRelName  = FileRelName  { unFileRelName  :: FilePath } deriving (Show, Eq, Ord)
data FileAbsName  = FileAbsName  { unFileAbsName  :: FilePath } deriving (Show, Eq, Ord)
data DirRelName   = DirRelName   { unDirRelName   :: FilePath } deriving (Show, Eq, Ord)
data DirAbsName   = DirAbsName   { unDirAbsName   :: FilePath } deriving (Show, Eq, Ord)

type Hashes = Map.Map FileRelName (Maybe (Digest MD5))
