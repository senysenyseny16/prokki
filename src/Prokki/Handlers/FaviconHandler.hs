module Prokki.Handlers.FaviconHandler (faviconHandler) where

import Control.Monad.IO.Class (MonadIO)
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, responseLBS)
import Prokki.Utils (favicon)

faviconHandler :: (MonadIO m) => Request -> m Response
faviconHandler _ = do
  pure $ responseLBS status200 [("Content-Type", "image/x-icon")] favicon
