{-# LANGUAGE OverloadedStrings #-}
module HB.Types where

import Data.Aeson
import Data.Aeson.Types (parse)
import Control.Monad
import qualified Data.Vector as V
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Data.Text as T

import qualified Debug.Trace as D

isSuccess :: Result a -> Bool
isSuccess (Success a) = True
isSuccess _ = False

fromSuccess :: Result a -> a
fromSuccess (Success a) = a

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

data DownloadStruct = DownloadStruct {
    ds_name       :: Maybe String -- "Download" or "Patch"
  , ds_file_size  :: Maybe Int
  , ds_timestamp  :: Maybe Int
  , ds_human_size :: Maybe String
  , ds_sha1       :: Maybe String
  , ds_md5        :: Maybe String
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
