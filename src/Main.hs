{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import Network.HTTP.Client.Utils
import System.IO
import System.Environment
import System.Directory
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as B8

import HB.Utils
import HB.Types

import qualified Debug.Trace as D

main = do
  args <- getArgs
  when (length args < 2) $ fail "WUT PLTFRM?!?? WHRE TO??!?!??"
  let platform = strToPlatform' . head $ args
      path = args !! 1
  (username, password) <- credentials

  withManager tlsManagerSettings $ \m -> do
    res <- auth m username password >>= click m (parseUrl "https://www.humblebundle.com/home")
    let keys' = getKeys (responseBody res)
    when (not . isJust $ keys') $ error "Couldn't find gamekeys"

    let Just keys = keys'

    forM_ keys $ \key -> do
      resp <- fetch m (responseCookieJar res) key
      let bundle = eitherDecode (responseBody resp) :: Either String Bundle
      guard $ isRight bundle

      let
        Right bundle' = bundle
        f dl = platform == All || Platform' (dl_platform dl) == platform
        dls = [ (sp_human_name sp, dl_machine_name dl, ds_human_size ds, fromJust u)
              | sp <- subproducts bundle'
              , dl <- sp_downloads sp, f dl
              , ds <- dl_download_struct dl
              , let u = dsu_web (ds_url ds)
              , isJust u ]
      forM_ dls $ \(name, mname, size, url) -> do
        putStrLn $ name ++ if isJust size
                           then " (" ++ fromJust size ++ ")"
                           else ""
        let dir = concat [path, "/", name]
        createDirectoryIfMissing True dir
        h <- openFile (concat [dir, "/", mname]) WriteMode
        download m url h
    -- print dls
    return ()

  return ()

  where
    keysFile = "bundles.txt"

isRight (Right _) = True
isRight _ = False

data Platform' = Platform' Platform | All
  deriving (Show, Eq)

strToPlatform' s = case s of
                     "All" -> All
                     a -> Platform' (read a)


-- Logout is GET to
-- https://www.humblebundle.com/logout?goto=/

