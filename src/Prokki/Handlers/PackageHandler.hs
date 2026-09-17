module Prokki.Handlers.PackageHandler (packageHandler) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newEmptyTMVar, putTMVar, readTMVar, readTVar, readTVarIO, writeTVar)
import Control.Monad.Catch (MonadMask, finally)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, runMaybeT)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Char (isHexDigit)
import qualified Data.Map as M
import qualified Data.OrdPSQ as PSQ
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Types (status302)
import Network.Wai (Request, Response, responseLBS)
import Prokki.Domain (packageCache, packageKeyS3, packageUpstream)
import Prokki.Env (WithManager, WithPGConnectionPool, WithPackageCache, WithPackageCacheMaxSize, WithS3, WithUploadRegistry, grab)
import Prokki.Handlers.ErrorHandler (errorHandler)
import Prokki.Storage.Postgres (lookupCacheEntryPg, markPackageCachedPg)
import Prokki.Storage.S3 (presignObjectS3)
import Prokki.Storage.S3.Stream (streamPackageToS3)
import Prokki.Types.CacheEntry
import Prokki.Types.Config
import Prokki.Types.Domain
import Prokki.Types.PackageEntry
import Prokki.Types.PackageRef

-- | The handler caches packages, including their metadata.
-- If a package is already cached, the handler returns it directly;
-- otherwise, it simultaneously initiates downloading the package from the index,
-- caching it, and delivering it to the client.
packageHandler ::
  ( MonadResource m,
    MonadMask m,
    WithManager env m,
    WithPGConnectionPool env m,
    WithS3 env m,
    WithUploadRegistry env m,
    WithPackageCache env m,
    WithPackageCacheMaxSize env m
  ) =>
  Request ->
  Index ->
  Path ->
  m Response
packageHandler req Index {..} reqPath = do
  pkgCache <- grab @(TVar PackageCache)
  PackageCacheMaxSize maxSize <- grab @PackageCacheMaxSize
  result <- runMaybeT $ do
    (pkg, kind) <- hoistMaybe (parsePackagePath indexName reqPath)
    let key = (pkgIndexName pkg, pkgUrlToken pkg)
    fromCache <- liftIO $ PSQ.lookup key <$> readTVarIO pkgCache
    (entry, count) <- case fromCache of
      Just (count, entry) -> pure (entry, count)
      Nothing -> do
        entry <- MaybeT (lookupCacheEntryPg (pkgIndexName pkg) (pkgUrlToken pkg))
        pure (entry, 0)
    pure (pkg, kind, entry, count)

  let serve pkg kind entry count = do
        let key = (pkgIndexName pkg, pkgUrlToken pkg)
        let cacheEntry entry' = liftIO $ atomically $ modifyTVar' pkgCache (insertEvicting maxSize key (count + 1) entry')
        case packageCache kind entry of
          Just cachedKey -> do
            cacheEntry entry
            redirectToS3 cachedKey (pkgFilename pkg)
          Nothing -> do
            let keyS3 = packageKeyS3 kind (pkgUrlToken pkg)
            let updEntry = case kind of
                  Package -> entry {ceS3Key = Just keyS3}
                  Metadata -> entry {ceMetadataS3Key = Just keyS3}
            startedAt <- liftIO getCurrentTime
            let info = UploadInfo (pkgIndexName pkg) (pkgFilename pkg) startedAt
            withSingleUpload keyS3 info $ do
              storePackage indexName entry (pkgUrlToken pkg) kind
              cacheEntry updEntry
            redirectToS3 keyS3 (pkgFilename pkg)

  case result of
    Nothing -> errorHandler req
    Just (pkg, kind, entry, count) -> serve pkg kind entry count
  where
    redirectToS3 keyS3 filename = do
      url <- presignObjectS3 keyS3 filename
      pure $ responseLBS status302 [("Location", url)] ""

parsePackagePath :: IndexName -> [T.Text] -> Maybe (PackageRef, PackageKind)
parsePackagePath idx segs = case segs of
  ["packages", _xx, _yy, token, filename]
    | validToken token ->
        let kind = if ".metadata" `T.isSuffixOf` filename then Metadata else Package
         in Just (PackageRef idx token filename, kind)
  _ -> Nothing
  where
    validToken t = T.length t == 32 && T.all isHexDigit t

storePackage ::
  (MonadIO m, WithManager env m, WithS3 env m, WithPGConnectionPool env m) =>
  IndexName ->
  CacheEntry ->
  T.Text ->
  PackageKind ->
  m ()
storePackage idx entry token kind = do
  let upstreamUrl = packageUpstream kind (ceUpstreamUrl entry)
      keyS3 = packageKeyS3 kind token
  len <- streamPackageToS3 upstreamUrl keyS3
  markPackageCachedPg idx token keyS3 kind len

withSingleUpload :: (MonadIO m, MonadMask m, WithUploadRegistry env m) => T.Text -> UploadInfo -> m () -> m ()
withSingleUpload key info action = do
  registry <- grab @(TVar UploadRegistry)
  decision <- liftIO $ atomically $ do
    m <- readTVar registry
    case M.lookup key m of
      Just (_, tmvar) -> pure (Right tmvar)
      Nothing -> do
        tmvar <- newEmptyTMVar
        writeTVar registry (M.insert key (info, tmvar) m)
        pure (Left tmvar)
  case decision of
    Right tmvar ->
      liftIO $ atomically $ readTMVar tmvar
    Left tmvar ->
      action `finally` liftIO (atomically $ putTMVar tmvar () >> modifyTVar' registry (M.delete key))
