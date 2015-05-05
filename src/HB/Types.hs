{-# LANGUAGE OverloadedStrings #-}
module HB.Types where

import qualified Data.ByteString.Base16 as B16
import           Data.Text.Encoding (encodeUtf8)
import           Data.Aeson
import           Control.Lens
import           Crypto.Hash
import qualified Data.Text as T

_Digest :: HashAlgorithm a => Prism' Value (Digest a)
_Digest = prism' undefined strToDigest
  where
    strToDigest (String s) = digestFromByteString . fst . B16.decode . encodeUtf8 $ s
    strToDigest _ = Nothing

data Platform = Windows | Mac | Linux | Android | Audio | Ebook | Asmjs
     deriving (Show, Read, Eq)

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
             , sha1        :: Maybe (Digest SHA1)
             , md5         :: Maybe (Digest MD5)
             } deriving (Show, Eq)
instance Ord DL where
  dl1 `compare` dl2 = (fsize dl1) `compare` (fsize dl2)

data Platform' = Platform' Platform | All
  deriving (Show, Eq)

strToPlatform' s = case s of
  "All" -> All
  a -> Platform' (read a)
