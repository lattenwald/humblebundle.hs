{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module HB.Types where

import           Control.Lens
import           Crypto.Hash
import           Data.Aeson
import qualified Data.ByteString.Base16 as B16
import           Data.Convertible
import qualified Data.Map               as Map
import qualified Data.Text              as T
import           Data.Typeable
import           Database.HDBC

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

newtype FileBaseName = FileBaseName { unFileBaseName :: FilePath } deriving (Show, Eq, Ord)
newtype FileRelName  = FileRelName  { unFileRelName  :: FilePath } deriving (Show, Eq, Ord)
newtype FileAbsName  = FileAbsName  { unFileAbsName  :: FilePath } deriving (Show, Eq, Ord)
newtype DirRelName   = DirRelName   { unDirRelName   :: FilePath } deriving (Show, Eq, Ord)
newtype DirAbsName   = DirAbsName   { unDirAbsName   :: FilePath } deriving (Show, Eq, Ord)

type Hashes = Map.Map FileRelName (Maybe (Digest MD5))

-- SqlValue instances
instance Convertible SqlValue FileRelName where
  safeConvert = fmap FileRelName . safeConvert

instance Convertible FileRelName SqlValue where
  safeConvert = safeConvert . unFileRelName

instance (HashAlgorithm a, Typeable a) => Convertible SqlValue (Digest a) where
  safeConvert v = do
    inp <- safeConvert v
    case (digestFromByteString . fst . B16.decode) inp of
      Nothing -> convError "Cannot convert to Digest" inp
      Just d -> return d

instance (HashAlgorithm a, Typeable a) => Convertible (Maybe (Digest a)) SqlValue where
  safeConvert inp@Nothing  = convError "Cannot convert Nothing" inp
  safeConvert (Just d) = safeConvert . digestToHexByteString $ d
