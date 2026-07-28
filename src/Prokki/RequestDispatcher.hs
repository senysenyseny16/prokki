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
import Prokki.Handlers.IndexRootHandler (indexRootHandler)
import Prokki.Handlers.IndexesHandler (indexesHandler)
import Prokki.Handlers.PackageHandler (packageHandler)
import Prokki.Handlers.ProjectHandler (projectHandler)
import Prokki.Monad (Prokki)
import Prokki.Types.Domain (IndexName (..), Indexes, RequestCounters)
import Prokki.Utils (packageExts)

requestDispatcher :: Request -> Prokki Response
requestDispatcher req = do
  indexes <- grab @Indexes
  case pathInfo req of
    [] -> pure $ responseLBS status301 [(hLocation, "/indexes")] "Redirecting to /indexes"
    ["favicon.ico"] -> faviconHandler req
    ["indexes"] -> indexesHandler req -- page with proxied indexes
    (index : path) ->
      -- scheme://host/index/*, where * is path
      maybe (errorHandler req) dispatch (M.lookup (IndexName index) indexes)
      where
        dispatch index'
          | isIndexRoot path = countRequest "index" >> indexRootHandler index' -- index root (page)
          | isProject path = countRequest "project" >> projectHandler req index' path -- index (page)
          | isPackage path = countRequest "package" >> packageHandler req index' path -- package (file)
          | otherwise = errorHandler req

countRequest :: (MonadIO m, WithRequestCounters env m) => T.Text -> m ()
countRequest key = do
  requestCounters <- grab @(TVar RequestCounters)
  liftIO $ atomically $ modifyTVar' requestCounters (M.insertWith (+) key 1)

isPackage :: [T.Text] -> Bool
isPackage [] = False
isPackage xs = any (`T.isSuffixOf` last xs) packageExts

isProject :: [T.Text] -> Bool
isProject xs = case filter (not . T.null) xs of
  [_] -> True
  _ -> False

isIndexRoot :: [T.Text] -> Bool
isIndexRoot = all T.null
