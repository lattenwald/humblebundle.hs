{-# LANGUAGE OverloadedStrings #-}
module HB.Types where

import           Data.Aeson
import           Data.Aeson.Types (parse)
import qualified Data.ByteString.Base16 as B16
import qualified Data.Vector as V
import           Crypto.Hash
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T

import qualified Debug.Trace as D

isSuccess :: Result a -> Bool
isSuccess (Success _) = True
isSuccess _ = False

fromSuccess :: Result a -> a
fromSuccess (Success a) = a
fromSuccess _ = error "Not success"

data Platform = Windows | Mac | Linux | Android | Audio | Ebook | Asmjs
     deriving (Show, Read, Eq)
instance FromJSON Platform where
  parseJSON (String s) = case s of
                           "windows" -> return Windows
                           "mac"     -> return Mac
                           "linux"   -> return Linux
                           "android" -> return Android
                           "audio"   -> return Audio
                           "ebook"   -> return Ebook
                           "asmjs"   -> return Asmjs
                           a -> fail $ "Don't know that platform: " ++ T.unpack a
  parseJSON a = fail ((show a) ++ " is not a String")

data DSUrl = DSUrl {
    dsu_web        :: Maybe String
  , dsu_bittorrent :: Maybe String
  } deriving Show
-- makeLenses ''DSUrl
instance FromJSON DSUrl where
  parseJSON (Object v) =
    DSUrl <$> v .:? "web"
          <*> v .:? "bittorrent"
  parseJSON a = fail ((show a) ++ " is not an Object")

instance (HashAlgorithm a) => FromJSON (Digest a) where
  parseJSON (String s) =
    case digestFromByteString . fst . B16.decode . encodeUtf8 $ s of
      Nothing -> fail "Invalid hash"
      Just h -> return h
  parseJSON a = fail ((show a) ++ " is not a digest String")

data DLType = DLTDownload | DLTTablet | DLTPatch | DLTIntelOnly
  deriving (Show, Eq)
instance FromJSON DLType where
  parseJSON (String "Intel Only") =  return DLTIntelOnly
  parseJSON (String "Patch") = return DLTPatch
  parseJSON (String s) | "Download Tablet" `T.isPrefixOf` s = return DLTTablet
  parseJSON (String "Download") = return DLTDownload
  parseJSON (String s) = fail $ D.traceShowId $ "Unknown DLType: " ++ T.unpack s
  parseJSON a = fail ((show a) ++ " is not a String")

data DownloadStruct = DownloadStruct {
    ds_type       :: Maybe DLType -- called "name" in JSON
  , ds_file_size  :: Maybe Int
  , ds_timestamp  :: Maybe Int
  , ds_human_size :: Maybe String
  , ds_sha1       :: Maybe (Digest SHA1)
  , ds_md5        :: Maybe (Digest MD5)
  , ds_url        :: DSUrl
  } deriving Show
-- makeLenses ''DownloadStruct
instance FromJSON DownloadStruct where
  parseJSON (Object v) =
    DownloadStruct <$> v .:? "name"
                   <*> v .:? "file_size"
                   <*> v .:? "timestamp"
                   <*> v .: "human_size"
                   <*> v .:? "sha1"
                   <*> v .:? "md5"
                   <*> v .: "url"
  parseJSON a = fail ((show a) ++ " is not an Object")

data Download = Download {
    dl_platform :: Platform
  , dl_machine_name :: String
  , dl_download_struct :: [DownloadStruct]
  } deriving Show
-- makeLenses ''Download
instance FromJSON Download where
  parseJSON (Object v) = do
    downloadStructs <- v .: "download_struct"
    case downloadStructs of
      Array ds -> do
        let parsed = V.toList
                   . V.map fromSuccess
                   . V.filter isSuccess
                   . V.map (parse parseJSON)
                   $ ds
        Download <$> v .: "platform"
                 <*> v .: "machine_name"
                 <*> return parsed
      _ -> fail "Expected download_struct to be an array"
  parseJSON a = fail ((show a) ++ " is not an Object")

data Subproduct = Subproduct {
    sp_machine_name :: String
  , sp_url          :: String
  , sp_human_name   :: String
  , sp_icon         :: String
  , sp_downloads    :: [Download]
  } deriving Show
-- makeLenses ''Subproduct
instance FromJSON Subproduct where
  parseJSON (Object v) =
    Subproduct <$> v .: "machine_name"
               <*> v .: "url"
               <*> v .: "human_name"
               <*> v .: "icon"
               <*> v .: "downloads"
  parseJSON a = fail ((show a) ++ " is not an Object")

data Bundle = Bundle {
  subproducts :: [Subproduct]
  } deriving Show
-- makeLenses ''Bundle
instance FromJSON Bundle where
  parseJSON (Object v) =
    Bundle <$> v .: "subproducts"
  parseJSON a = fail ((show a) ++ " is not an Object")

data DL = DL { hname :: String
             , mname :: String
             , platform :: Platform
             , dltype :: Maybe DLType
             , url :: String
             , hsize :: Maybe String
             , fsize :: Maybe Int
             , sha1 :: Maybe (Digest SHA1)
             , md5 :: Maybe (Digest MD5)
             } deriving (Show, Eq)
instance Ord DL where
  dl1 `compare` dl2 = (fsize dl1) `compare` (fsize dl2)

data Platform' = Platform' Platform | All
  deriving (Show, Eq)

strToPlatform' s = case s of
                     "All" -> All
                     a -> Platform' (read a)
