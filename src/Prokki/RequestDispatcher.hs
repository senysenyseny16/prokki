module Prokki.RequestDispatcher (requestDispatcher) where

import qualified Data.Map as M
import Network.HTTP.Types (hLocation, status301)
import Network.Wai (Request, Response, pathInfo, responseLBS)
import Prokki.Env (grab)
import Prokki.Handlers.ErrorHandler (errorHandler)
import Prokki.Handlers.FaviconHandler (faviconHandler)
import Prokki.Handlers.IndexHandler (indexHandler)
import Prokki.Handlers.IndexesHandler (indexesHandler)
import Prokki.Handlers.PackageHandler (packageHandler)
import Prokki.Monad (Prokki)
import Prokki.Type (Indexes)
import Prokki.Utils (isPackage)

requestDispatcher :: Request -> Prokki Response
requestDispatcher req = do
  indexes <- grab @Indexes
  case pathInfo req of
    [] -> pure $ responseLBS status301 [(hLocation, "/indexes")] "Redirecting to /indexes"
    ["favicon.ico"] -> faviconHandler req
    ["indexes"] -> indexesHandler req -- page with proxied indexes
    (index : path) ->
      -- scheme://host/index/*, where * is path
      maybe (errorHandler req) dispatch (M.lookup index indexes)
      where
        dispatch index'
          | isPackage path = packageHandler req index' path -- package (file)
          | otherwise = indexHandler req index' path -- html
