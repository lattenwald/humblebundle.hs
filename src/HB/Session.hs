module HB.Session where

import           Data.Binary as Bin
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Maybe
import           HB.Derive ()
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.Wreq.Session
import           System.Directory (doesFileExist)

cookiesFile = "cookies"

loadCookies' :: IO (Maybe CookieJar)
loadCookies' = do
  exists <- doesFileExist cookiesFile
  if exists
     then fmap Just $ BL8.readFile cookiesFile >>= return . Bin.decode
     else pure Nothing

saveCookies' :: CookieJar -> IO ()
saveCookies' = BL8.writeFile cookiesFile . Bin.encode

withSession' :: (Session -> IO a) -> IO a
withSession' f = do
  cookies <- loadCookies
  withSessionControl
    (Just $ fromMaybe (createCookieJar []) cookies)
    tlsManagerSettings
    f

saveCookies :: CookieJar -> IO ()
saveCookies = writeFile cookiesFile . show

loadCookies :: IO (Maybe CookieJar)
loadCookies = do
  exists <- doesFileExist cookiesFile
  if exists
     then (Just . read) <$> readFile cookiesFile
     else pure Nothing
