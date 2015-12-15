{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.Utils (
    module Network.HTTP.Client
  , module Network.HTTP.Client.TLS
  , click, clickWith, toPost, addPostFields, insertCsrf
) where
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString as B
import Data.List (find)

click m mnext = clickWith m mnext id

clickWith m mnext f res = do
  next <- mnext
  let next' = f $ next { cookieJar = Just (responseCookieJar res)}
  httpLbs next' m

toPost req = req { method = "POST" }

addPostFields stuff req = req {
  requestBody = RequestBodyBS $ B.intercalate "&" . map (\(a, b) -> B.concat [a, "=", b]) $ stuff
  }

insertCsrf req = req { requestBody = RequestBodyBS $ B.concat [ oldBody
                                                               , "&_le_csrf_token="
                                                               , cookie_value csrf_cookie ] }
  where
    Just jar = cookieJar req
    Just csrf_cookie = find ((== "csrf_cookie") . cookie_name) . destroyCookieJar $ jar
    RequestBodyBS oldBody = requestBody req
