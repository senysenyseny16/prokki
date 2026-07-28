{-# LANGUAGE ScopedTypeVariables #-}

module Prokki.Handlers.ProjectHandler (projectHandler) where

import Colog (Message, WithLog, log, pattern D, pattern W)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVarIO)
import Control.Exception (Exception)
import Control.Monad.Catch (MonadCatch, catch, handle, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.OrdPSQ as PSQ
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time (diffUTCTime, getCurrentTime)
import qualified Network.HTTP.Conduit as C
import Network.HTTP.Simple (httpSink, setRequestCheckStatus)
import Network.HTTP.Types (status200, status404, status502, statusCode)
import Network.Wai (Request, Response, responseLBS)
import Prokki.Domain (indexUpstreamUrl)
import Prokki.Env (WithPGConnectionPool, WithProjectCache, WithSettings, grab)
import Prokki.Handlers.ErrorHandler (errorHandler)
import Prokki.SimpleIndex.Parser (parseProjectDocument)
import Prokki.SimpleIndex.Render (renderProjectPage)
import Prokki.SimpleIndex.Utils (toPackageEntry)
import Prokki.Storage.Postgres (getProjectEntriesPg, isProjectSyncPg, markProjectFetchedPg, setPackageEntriesPg)
import Prokki.Types.Config
import Prokki.Types.Domain
import Prokki.Types.PackageEntry
import Text.HTML.DOM (sinkDoc)
import Prelude hiding (log)

data ProjectNotFound = ProjectNotFound deriving (Show)

instance Exception ProjectNotFound

-- | The handler fully replicates the "simple repository API",
-- replacing links from the original repository with its own.
-- https://packaging.python.org/en/latest/specifications/simple-repository-api/#base-html-api
projectHandler ::
  ( MonadIO m,
    MonadCatch m,
    WithLog env Message m,
    WithPGConnectionPool env m,
    WithProjectCache env m,
    WithSettings env m
  ) =>
  Request ->
  Index ->
  Path ->
  m Response
projectHandler req index@Index {..} reqPath = do
  case parseProject reqPath of
    Nothing -> errorHandler req
    Just project -> do
      now <- liftIO getCurrentTime
      baseUrl <- grab @ProkkiBaseUrl
      ProjectCacheTtl ttl <- grab @ProjectCacheTtl
      projectCache <- grab @(TVar ProjectCache)
      ProjectCacheMaxSize maxSize <- grab @ProjectCacheMaxSize

      let key = (indexName, project)
          render = renderProjectPage baseUrl indexName project

          cacheWith count fetchedAt html =
            liftIO $ atomically $ modifyTVar' projectCache (insertEvicting maxSize key count (fetchedAt, html))

          respond count entries = do
            let html = render entries
            cacheWith count now html
            pure $ htmlResponse html

          respondOrFallback count stale = case stale of
            Just (fetchedAt, html) -> do
              log W $ "Upstream unavailable, serving stale cache for project " <> project <> " in index " <> unIndexName indexName
              cacheWith (count + 1) fetchedAt html
              pure $ htmlResponse html
            Nothing -> do
              entries <- getProjectEntriesPg indexName project
              if null entries then pure errorResponse else respond (count + 1) entries

          syncOrFallback count stale = do
            isSync <- isProjectSyncPg indexName project ttl
            fetched <-
              if isSync
                then Just <$> getProjectEntriesPg indexName project
                else trySyncProject index reqPath project
            maybe (respondOrFallback count stale) (respond (count + 1)) fetched

      fromCache <- liftIO $ PSQ.lookup key <$> readTVarIO projectCache
      handle (\(_ :: ProjectNotFound) -> pure notFoundResponse) $
        case fromCache of
          Just (count, (fetchedAt, html)) | diffUTCTime now fetchedAt < ttl -> do
            cacheWith (count + 1) fetchedAt html
            pure $ htmlResponse html
          Just (count, (fetchedAt, html)) -> syncOrFallback count (Just (fetchedAt, html))
          Nothing -> syncOrFallback 0 Nothing
  where
    htmlResponse html =
      responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] (TLE.encodeUtf8 html)
    errorResponse =
      responseLBS status502 [("Content-Type", "text/plain; charset=utf-8")] "Upstream unavailable and no cached data"
    notFoundResponse =
      responseLBS status404 [("Content-Type", "text/plain; charset=utf-8")] "Not found"

trySyncProject ::
  (MonadIO m, MonadCatch m, WithLog env Message m, WithPGConnectionPool env m) =>
  Index ->
  Path ->
  T.Text ->
  m (Maybe [PackageEntry])
trySyncProject index@Index {..} reqPath project =
  (Just <$> syncProjectFromUpstream index reqPath project)
    `catch` \(exc :: C.HttpException) -> case exc of
      C.HttpExceptionRequest _ (C.StatusCodeException resp _)
        | statusCode (C.responseStatus resp) == 404 -> throwM ProjectNotFound
      _ -> do
        log W $ "Upstream sync failed for project " <> project <> " in index " <> unIndexName indexName <> ": " <> T.pack (show exc)
        pure Nothing

syncProjectFromUpstream ::
  (MonadIO m, WithPGConnectionPool env m, WithLog env Message m) =>
  Index ->
  Path ->
  T.Text ->
  m [PackageEntry]
syncProjectFromUpstream index@Index {..} reqPath project = do
  let url = indexUpstreamUrl index <> T.intercalate "/" reqPath
  request <- liftIO $ setRequestCheckStatus <$> C.parseRequest (T.unpack url)
  doc <- liftIO $ httpSink request (const sinkDoc)
  let links = parseProjectDocument indexOrigin doc
      entries = map (toPackageEntry indexName project) links
  setPackageEntriesPg entries
  markProjectFetchedPg indexName project

  log D $
    "Fetched "
      <> T.pack (show (length entries))
      <> " package entries for project "
      <> project
      <> " from index (upstream) "
      <> unIndexName indexName

  pure entries

parseProject :: [T.Text] -> Maybe T.Text
parseProject path =
  case filter (not . T.null) path of
    [project] -> Just project
    _ -> Nothing
