{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Prokki.RequestDispatcher (requestDispatcher) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Resource (ResIO)
import qualified Network.HTTP.Conduit as C
import Network.Wai (Request, Response, pathInfo)
import Prokki.Handlers.ErrorHandler (errorHandler)
import Prokki.Handlers.IndexHandler (indexHandler)
import Prokki.Handlers.PackagesHandler (packagesHandler)
import Prokki.Handlers.CacheHandler (cacheHandler)  -- Import the cacheHandler
import Prokki.Settings (Settings (..))

requestDispatcher :: Request -> C.Manager -> Settings -> ResIO Response
requestDispatcher req manager settings@Settings{..} =
  case pathInfo req of
    ("simple" : _)    -> liftIO $ indexHandler req manager index
    ("packages" : _)  -> packagesHandler req manager index cache
    ("cache" : subPath) -> liftIO $ cacheHandler subPath req manager cache  -- Delegate to cacheHandler
    _                  -> liftIO $ errorHandler req manager
