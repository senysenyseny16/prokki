{-# LANGUAGE FlexibleContexts #-}

module Prokki.RequestDispatcher (requestDispatcher) where

-- import Prokki.Handlers.PackagesHandler (packagesHandler)
--

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Network.Wai (Request, Response, pathInfo)
import Prokki.Env (Env (..))
import Prokki.Handlers.ErrorHandler (errorHandler)
import Prokki.Handlers.SimpleHandler (simpleHandler)
import Prelude hiding (log)

requestDispatcher :: (MonadIO m, MonadReader (Env m) m) => Request -> m Response
requestDispatcher req = do
  case pathInfo req of
    ("simple" : _) -> simpleHandler req
    -- ("packages" : _) -> packagesHandler req
    _ -> errorHandler req
