{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
module HB.Derive where

import           Crypto.Hash
import           Data.Binary
import           Data.Byteable
import           Data.DeriveTH
import           Data.Time
import           HB.Types
import           Network.HTTP.Client.Internal

instance Binary DiffTime where
  get = secondsToDiffTime <$> get
  put = put

$( derive makeBinary ''Day )
$( derive makeBinary ''UTCTime )
$( derive makeBinary ''Cookie )
$( derive makeBinary ''CookieJar )

instance Binary (Digest MD5) where
  put = put . toBytes
  get = maybe (fail "cannot decode to bytestring") pure . digestFromByteString =<< get

$( derive makeBinary ''FileRelName )
$( derive makeBinary ''Hashes )
