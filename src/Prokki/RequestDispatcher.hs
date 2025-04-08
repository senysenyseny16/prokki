{-# LANGUAGE OverloadedStrings #-}

module Prokki.RequestDispatcher (requestDispatcher) where

import Network.Wai (Request, Response, pathInfo)
import Prokki.Handlers.ErrorHandler (errorHandler)
import Prokki.Handlers.IndexHandler (indexHandler)
import Prokki.Handlers.PackagesHandler (packagesHandler)

requestDispatcher :: Request -> IO Response
requestDispatcher req =
  case pathInfo req of
    ("simple" : _) -> indexHandler req
    ("packages" : _) -> packagesHandler req
    _ -> errorHandler req
