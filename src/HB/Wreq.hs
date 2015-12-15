{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
module HB.Wreq where

import           Control.Exception
import           Control.Lens hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.ByteString.Lens
import           Data.List (sort)
import           Data.Maybe
import           Data.Text (Text)
import qualified Network.HTTP.Client as Client
import           Network.Wreq hiding (post, get)
import           Network.Wreq.Session

-- 1. auth
-- 2. /home, fetch keys
-- 3. fetch orders in parallel
-- 4. fetch all stuff with invalid or missing hash in parallel

data HBException = InvalidCredentials | UnknownException String
  deriving Show
instance Exception HBException

type Resp = Response BL8.ByteString

isInfixOf substr str = substr `B8.isInfixOf` (B.concat . BL8.toChunks $ str)

hbInit :: Session -> IO Resp
hbInit sess = get sess "https://www.humblebundle.com/home"

hbAuth :: Session -> Resp -> IO Resp
hbAuth sess resp =
  if "class='account-login'" `isInfixOf` (resp ^. responseBody)
     then do
       r <- post sess "https://www.humblebundle.com/login/captcha" ([] :: [FormParam])
       (user, pass) <- getCredentials
       post sess "https://www.humblebundle.com/processlogin"
         [ "username" := user
         , "password" := pass
         , "_le_csrf_token" := r ^. responseCookie "csrf_cookie" . cookieValue ]
       get sess "https://www.humblebundle.com/home"
     else pure resp

passGuard :: Session -> Resp -> IO Resp
passGuard sess resp =
  if "<h1>Verify this Browser</h1>" `isInfixOf` (resp ^. responseBody)
     then do
       code <- getCode
       post sess "https://www.humblebundle.com/user/humbleguard"
         [ "qs" := ("" :: B.ByteString)
         , "code" := code
         , "goto" := ("/home" :: B8.ByteString) ]
     else pure resp

hbKeys :: Session -> IO [String]
hbKeys sess = do
  r <- get sess "https://www.humblebundle.com/home"
  case getKeys (r ^. responseBody) of
    Nothing -> pure []
    Just keys -> pure keys

getKeys :: BL8.ByteString -> Maybe [String]
getKeys body = k
  where
    searchString = "gamekeys = "
    gamekeysLocation = fromIntegral . (length searchString +) <$>
                       bsSearch searchString body
    gamekeys = BL8.takeWhile (/= ']') . flip BL8.drop body <$> gamekeysLocation
    k = read . (++ "]") . BL8.unpack <$> gamekeys

bsSearch :: String -> BL8.ByteString -> Maybe Int
bsSearch term str = findConsecutive indices
  where
    indices = map (map fromIntegral . flip BL8.elemIndices str) term
    findConsecutive [] = Nothing
    findConsecutive (xs:rest) = case sort . filter isJust . map (`findConsecutive'` rest) $ xs of
                                     [] -> Nothing
                                     (x:_) -> subtract (length rest) <$> x
    findConsecutive' i [] = Just i
    findConsecutive' _ ([]:_) = Nothing
    findConsecutive' i ((x:xs):rest) = case (i+1) `compare` x of
                                            EQ -> findConsecutive' x rest
                                            GT -> findConsecutive' i (xs:rest)
                                            LT -> Nothing

hbCatch :: Client.HttpException -> IO a
hbCatch (Client.StatusCodeException status hdr _) = do
  let err = show (status ^. statusCode) ++ " " ++ (status ^. statusMessage . unpackedChars)
      maybeErrorText = fmap snd $ listToMaybe . filter (\(a, _) -> a == "X-Response-Body-Start") $ hdr
      jsonError = preview (key "errors") =<< maybeErrorText
      errorText = maybe (err ++ " <unknown error>") show jsonError
  case fmap (== object ["username" .= ("Email and password don't match" :: Text)]) jsonError of
    Just True -> throw InvalidCredentials
    _         -> throw (UnknownException errorText)
hbCatch e = throw e

getCredentials = do
  putStr "Username: "
  user <- B8.getLine
  putStr "Password: "
  -- TODO no echo
  pass <- B8.getLine
  return (user, pass)

getCode = do
  putStrLn "You should have received an email, what's the code in there?"
  B8.getLine
