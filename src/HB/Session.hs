module HB.Session where

import Data.Maybe
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.Wreq.Session
import System.Directory (doesFileExist)

cookiesFile = "cookies"

loadCookies :: IO (Maybe CookieJar)
loadCookies = do
  exists <- doesFileExist cookiesFile
  if exists
     then (Just . read) <$> readFile cookiesFile
     else pure Nothing

withSession' :: (Session -> IO a) -> IO a
withSession' f = do
  cookies <- loadCookies
  withSessionControl
    (Just $ fromMaybe (createCookieJar []) cookies)
    tlsManagerSettings
    f

saveCookies :: CookieJar -> IO ()
saveCookies = writeFile cookiesFile . show
