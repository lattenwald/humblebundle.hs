import           Control.Concurrent.ParallelIO
import           Control.Exception
import           Control.Lens
import qualified Data.Map as Map
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.Wreq hiding (options, header)
import           Options.Applicative

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
main = execParser opts >>= run
  where
    opts = info (helper <*> options)
        (  fullDesc
        <> progDesc "Download binaries from HumbleBundle"
        <> header   "HumbleBundle downloader"
        )

run :: MainOptions -> IO ()
run opts = do
  let pl          = strToPlatform' $ optPlatform opts
      path        = DirAbsName $ optDestination opts
      hashStorage = FileRelName $ optHashStorage opts
  putStrLn $ "Getting hashes from " ++ show hashStorage ++ "..."
  hashes <- loadHashes hashStorage
  putStrLn $ "Total " ++ show (Map.size . getHashes $ hashes) ++ " hashes there"

  -- credentials
  (cookies, keys) <- handle hbCatch . withSession' $ \sess -> do
    resp <- hbInit sess >>=
            hbAuth sess
    let cookies = resp ^. responseCookieJar
    saveCookies cookies
    keys <- hbKeys sess
    pure (cookies, keys)

  m <- newManager tlsManagerSettings

  -- fetch all bundles data and extract download information
  putStrLn "Fetching keys..."
  bundles <- parallelInterleaved . map (fetch m cookies) $ keys
  let dls = filterPlatform pl . uniq . concatMap parseBundle $ bundles

  -- execute downloads
  putStrLn "Downloads on it's way..."
  newHashes <- parallelInterleaved
               . map (executeDownload m hashes path (optVerbose opts)) $ dls

  saveHashes (Hashes $ Map.fromList newHashes) hashStorage

  stopGlobalPool
  putStrLn "All done!"

-- Logout is GET to
-- https://www.humblebundle.com/logout?goto=/
