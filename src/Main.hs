import Network.HTTP.Client.Utils
import System.Environment
import Data.Maybe
import Control.Monad

import HB.Utils
import HB.Types

main = do
  -- arguments
  args <- getArgs
  when (length args < 2) $ fail "WUT PLTFRM?!?? WHRE TO??!?!??"
  let platform = strToPlatform' . head $ args
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
    bundles <- map fromRight . filter isRight <$>
      mapM (fetch m (responseCookieJar res)) keys
    let dls = extractDLs platform bundles

    -- execute downloads
    putStrLn "Downloads on it's way!"
    forM_ dls (executeDownload m path)
    -- forM_ (sort . filter (isJust . fsize) $ dls) (executeDownload m path)


-- Logout is GET to
-- https://www.humblebundle.com/logout?goto=/

