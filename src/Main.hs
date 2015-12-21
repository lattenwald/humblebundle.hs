{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Concurrent.ParallelIO
import           Control.Exception
import           Control.Lens
import qualified Data.Map as Map
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.Wreq hiding (options, header, get)
import           Network.Wreq.Session hiding (options)
import           Options.Applicative
import           System.IO

import           HB.Session
import           HB.Types
import           HB.Utils
import           HB.Wreq

data MainOptions = MainOptions { optVerbose :: Bool
                               , optPlatform :: String
                               , optDestination :: String
                               , optHashStorage :: String }

options :: Parser MainOptions
options = MainOptions <$>
  switch ( long "verbose"
          <> short 'v'
          <> help "be verbose" )
  <*>
  strOption ( long "platform"
             <> short 'p'
             <> value "Android"
             <> help "platform to download binaries for" )
  <*>
  strOption ( long "destination"
             <> short 'd'
             <> help "where to download binaries" )
  <*>
  strOption ( long "hashes"
             <> short 'h'
             <> value "hashes"
             <> help "file with hashes" )

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  execParser opts >>= runHB
  where
    opts = info (helper <*> options)
        (  fullDesc
        <> progDesc "Download binaries from HumbleBundle"
        <> header   "HumbleBundle downloader"
        )

runHB :: MainOptions -> IO ()
runHB opts = do
  let pl          = strToPlatform' $ optPlatform opts
      path        = DirAbsName $ optDestination opts
      hashStorage = FileRelName $ optHashStorage opts
  putStrLn $ "Getting hashes from " ++ show hashStorage ++ "..."
  hashes <- loadHashes hashStorage
  putStrLn $ "Total " ++ show (Map.size . getHashes $ hashes) ++ " hashes there"

  -- credentials
  bundles <- handle hbCatch . withSession' $ \sess -> do
    resp <- hbInit sess >>=
            hbAuth sess
    let cookies = resp ^. responseCookieJar
    saveCookies cookies
    keys <- hbKeys sess

    putStrLn "Fetching bundles info..."
    let urls = map ("https://www.humblebundle.com/api/v1/order/" ++) keys
    bundles :: [DL] <- fmap (uniq . concat)
                         . parallelInterleaved
                         . map (\u -> view responseBody <$> (asJSON =<< get sess u))
                         $ urls
    pure bundles

  -- fetch all bundles data and extract download information
  let dls = filterPlatform pl bundles

  -- execute downloads
  putStrLn "Downloads on it's way..."
  m <- newManager tlsManagerSettings
  newHashes <- parallelInterleaved
             . map (executeDownload m hashes path (optVerbose opts)) $ dls

  saveHashes (Hashes $ Map.fromList newHashes) hashStorage

  stopGlobalPool
  putStrLn "All done!"

-- Logout is GET to
-- https://www.humblebundle.com/logout?goto=/
