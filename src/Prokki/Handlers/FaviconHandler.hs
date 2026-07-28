module Prokki.Handlers.FaviconHandler (faviconHandler) where

import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, responseLBS)
import Prokki.Utils (favicon)

faviconHandler :: (Applicative m) => Request -> m Response
faviconHandler _ = pure $ responseLBS status200 [("Content-Type", "image/x-icon")] favicon
