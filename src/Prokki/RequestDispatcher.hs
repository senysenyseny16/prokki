{-# LANGUAGE OverloadedStrings #-}

module Prokki.RequestDispatcher (requestDispatcher) where

import qualified Network.HTTP.Conduit as C
import Network.Wai (Request, Response, pathInfo)
import Prokki.Handlers.ErrorHandler (errorHandler)
import Prokki.Handlers.IndexHandler (indexHandler)
import Prokki.Handlers.PackagesHandler (packagesHandler)
import Control.Monad.Trans.Resource (ResIO)
import Control.Monad.IO.Class (MonadIO(liftIO))

requestDispatcher :: C.Manager -> Request -> ResIO Response
requestDispatcher manager req =
  case pathInfo req of
    ("simple" : _) -> liftIO $ indexHandler manager req
    ("packages" : _) -> packagesHandler manager req
    _ -> liftIO $ errorHandler manager req
