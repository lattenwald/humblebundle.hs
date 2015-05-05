import Network.HTTP.Client.Utils
import System.Environment
import Control.Monad
import Control.Concurrent.ParallelIO
import qualified Data.Set as S

import HB.Utils
import HB.Types

main = do
  -- arguments
  args <- getArgs
  when (length args < 2) $ fail "WUT PLTFRM?!?? WHRE TO??!?!??"
  let pl = strToPlatform' . head $ args
      path = args !! 1

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
    putStrLn "Downloads on it's way!"
    parallelInterleaved . map (executeDownload m path) $ dls

    stopGlobalPool

uniq :: Ord a => [a] -> [a]
uniq = S.toList . S.fromList

-- Logout is GET to
-- https://www.humblebundle.com/logout?goto=/

