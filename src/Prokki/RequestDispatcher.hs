{-# LANGUAGE OverloadedStrings #-}

module Prokki.RequestDispatcher (requestDispatcher) where

import qualified Network.HTTP.Conduit as C
import Network.Wai (Request, Response, pathInfo)
import Prokki.Handlers.ErrorHandler (errorHandler)
import Prokki.Handlers.IndexHandler (indexHandler)
import Prokki.Handlers.PackagesHandler (packagesHandler)

requestDispatcher :: C.Manager -> Request -> IO Response
requestDispatcher manager req =
  case pathInfo req of
    ("simple" : _) -> indexHandler manager req
    ("packages" : _) -> packagesHandler manager req
    _ -> errorHandler manager req
