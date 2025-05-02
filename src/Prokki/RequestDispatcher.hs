module Prokki.RequestDispatcher (requestDispatcher) where

import Network.Wai (Request, Response, pathInfo)
import Prokki.Handlers.ErrorHandler (errorHandler)
import Prokki.Handlers.PackagesHandler (packagesHandler)
import Prokki.Handlers.SimpleHandler (simpleHandler)
import Prokki.Monad (Prokki)

requestDispatcher :: Request -> Prokki Response
requestDispatcher req = do
  case pathInfo req of
    ("simple" : _) -> simpleHandler req
    ("packages" : _) -> packagesHandler req
    _ -> errorHandler req
