import Network.HTTP.Client.Utils
import Control.Monad
import Control.Concurrent.ParallelIO
import qualified Data.Map as Map
import Options.Applicative

import HB.Utils
import HB.Types

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
  hashes <- getHashes hashStorage
  putStrLn $ "Total " ++ show (Map.size hashes) ++ " hashes there"

  -- credentials
  (username, password) <- credentials

  withManager tlsManagerSettings $ \m -> do
    -- authenticate and get /home to extract gamekeys
    putStrLn "Authenticating..."
    res <- auth m username password >>= click m (parseUrl "https://www.humblebundle.com/home")
    let keys' = getKeys (responseBody res)
    when (not . isJust $ keys') $ error "Couldn't find gamekeys"
    let Just keys = keys'

    -- fetch all bundles data and extract download information
    putStrLn "Fetching keys..."
    bundles <- parallelInterleaved . map (fetch m (responseCookieJar res)) $ keys
    let dls = filterPlatform pl . uniq . concat . map parseBundle $ bundles

    -- execute downloads
    putStrLn "Downloads on it's way..."
    newHashes <- parallelInterleaved
                 . map (executeDownload m hashes path (optVerbose opts)) $ dls

    saveHashes (Map.fromList newHashes) hashStorage

    stopGlobalPool
    putStrLn "All done!"

-- Logout is GET to
-- https://www.humblebundle.com/logout?goto=/

