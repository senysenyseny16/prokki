module Prokki.RequestDispatcher (requestDispatcher) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map as M
import qualified Data.Text as T
import Network.HTTP.Types (hLocation, status301)
import Network.Wai (Request, Response, pathInfo, responseLBS)
import Prokki.Env (WithRequestCounters, grab)
import Prokki.Handlers.ErrorHandler (errorHandler)
import Prokki.Handlers.FaviconHandler (faviconHandler)
import Prokki.Handlers.IndexHandler (indexHandler)
import Prokki.Handlers.IndexesHandler (indexesHandler)
import Prokki.Handlers.PackageHandler (packageHandler)
import Prokki.Monad (Prokki)
import Prokki.Type (Indexes, RequestCounters)
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
          | isPackage path = countRequest "package" >> packageHandler req index' path -- package (file)
          | otherwise = countRequest "index" >> indexHandler req index' path -- html

countRequest :: (MonadIO m, WithRequestCounters env m) => T.Text -> m ()
countRequest key = do
  requestCounters <- grab @(TVar RequestCounters)
  liftIO $ atomically $ modifyTVar' requestCounters (M.insertWith (+) key 1)
