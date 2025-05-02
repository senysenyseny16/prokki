module Prokki.Handlers.ErrorHandler (errorHandler) where

import Control.Monad.IO.Class (MonadIO)
import Network.HTTP.Types (status404)
import Network.Wai (Request, Response, responseLBS)

errorHandler :: (MonadIO m) => Request -> m Response
errorHandler _ = do
  pure $ responseLBS status404 [("Content-Type", "text/plain")] "404 Not Found"
