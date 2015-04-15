{-# LANGUAGE OverloadedStrings #-}
module HB.Utils where

import Data.List
import Data.Maybe
import System.IO
import Network.HTTP.Client.Utils
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as B8

import Pipes
import Pipes.HTTP
import qualified Pipes.ByteString as PB

credentials :: IO (B.ByteString, B.ByteString)
credentials = do
  h <- openFile "credentials" ReadMode
  username <- B.hGetLine h
  password <- B.hGetLine h
  return (username, password)

auth m username password = do
  res <- parseUrl "https://www.humblebundle.com"
    >>= flip httpNoBody m
    >>= clickWith m (parseUrl "https://www.humblebundle.com/login/captcha") toPost
    >>= clickWith m (parseUrl "https://www.humblebundle.com/processlogin") (
        insertCsrf
      . addPostFields [("username", username), ("password", password)]
      . toPost )
  return res

fetch m jar key = do
  req <- parseUrl ("https://www.humblebundle.com/api/v1/order/" ++ key)
  let req' = req { cookieJar = Just jar }
  httpLbs req' m

download m url h = do
  req <- parseUrl url
  withHTTP req m $ \resp ->
    runEffect $ responseBody resp >-> PB.toHandle h

getKeys :: B8.ByteString -> Maybe [String]
getKeys body = k
  where
    searchString = "gamekeys = "
    gamekeysLocation = fromIntegral . (length searchString +) <$>
                       bsSearch searchString body
    gamekeys = B8.takeWhile (/= ']') . flip B8.drop body <$> gamekeysLocation
    k = read . (++ "]") . B8.unpack <$> gamekeys


bsSearch :: String -> B8.ByteString -> Maybe Int
bsSearch term str = findConsecutive indices
  where
    indices = map (map fromIntegral . flip B8.elemIndices str) term
    findConsecutive [] = Nothing
    findConsecutive (xs:rest) = case sort . filter isJust . map (flip findConsecutive' rest) $ xs of
                                     [] -> Nothing
                                     (x:_) -> (subtract (length rest)) <$> x
    findConsecutive' i [] = Just i
    findConsecutive' i ([]:_) = Nothing
    findConsecutive' i ((x:xs):rest) = case (i+1) `compare` x of
                                            EQ -> findConsecutive' x rest
                                            GT -> findConsecutive' i (xs:rest)
                                            LT -> Nothing

