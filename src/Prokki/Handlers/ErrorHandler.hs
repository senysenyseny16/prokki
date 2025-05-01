{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

module Prokki.Handlers.ErrorHandler (errorHandler) where

import Colog (Message, WithLog, log, pattern W)
import Control.Monad.IO.Class (MonadIO)
import Network.HTTP.Types (status404)
import Network.Wai (Request, Response, responseLBS)
import Prokki.Utils (getPath)
import Prelude hiding (log)

errorHandler :: (MonadIO m, WithLog env Message m) => Request -> m Response
errorHandler req = do
  log W $ "Unexpected endpoint: " <> getPath req
  pure $ responseLBS status404 [("Content-Type", "text/plain")] "404 Not Found"
