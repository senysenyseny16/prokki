module Prokki.Storage.Postgres
  ( mkPgPool,
    runPg,
    checkConnPg,
    setPackageEntryPg,
    setPackageEntriesPg,
    lookupCacheEntryPg,
    markPackageCachedPg,
    cachedByIndexPg,
    isProjectSyncPg,
    markProjectFetchedPg,
    getProjectEntriesPg,
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Pool (defaultPoolConfig, newPool, withResource)
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Database.PostgreSQL.Simple (Only (..))
import qualified Database.PostgreSQL.Simple as PG
import Prokki.Env (WithPGConnectionPool, grab)
import Prokki.Storage.Postgres.Types (PGConnectionPool (..))
import Prokki.Types.CacheEntry (CacheEntry (..))
import Prokki.Types.Config (PGConfig (..))
import Prokki.Types.Domain (IndexName (..), Sha256)
import Prokki.Types.PackageEntry (PackageEntry (..), PackageKind (..))
import System.Environment (getEnv)

mkConnectInfo :: PGConfig -> IO PG.ConnectInfo
mkConnectInfo cfg = do
  user <- getEnv "PGUSER"
  pass <- getEnv "PGPASSWORD"
  pure
    PG.defaultConnectInfo
      { PG.connectHost = T.unpack (pgHost cfg),
        PG.connectPort = fromIntegral (pgPort cfg),
        PG.connectDatabase = T.unpack (pgDatabase cfg),
        PG.connectUser = user,
        PG.connectPassword = pass
      }

mkPgPool :: PGConfig -> IO PGConnectionPool
mkPgPool cfg = do
  connInfo <- mkConnectInfo cfg
  let connStr = PG.postgreSQLConnectionString connInfo <> sslModeParam (pgSecure cfg)
  pool <- newPool (defaultPoolConfig (PG.connectPostgreSQL connStr) PG.close 60 20)
  void $ withResource pool $ \conn -> PG.query_ conn "SELECT 1" :: IO [PG.Only Int] -- test connection
  pure (PGConnectionPool pool)

sslModeParam :: Bool -> ByteString
sslModeParam secure = " sslmode=" <> if secure then "require" else "disable"

runPg :: (MonadIO m, WithPGConnectionPool env m) => (PG.Connection -> IO a) -> m a
runPg action = do
  PGConnectionPool pool <- grab @PGConnectionPool
  liftIO $ withResource pool action

checkConnPg :: (MonadIO m, WithPGConnectionPool env m) => m ()
checkConnPg = runPg $ \conn -> void (PG.query_ conn "SELECT 1" :: IO [PG.Only Int])

setPackageEntryPg :: (MonadIO m, WithPGConnectionPool env m) => PackageEntry -> m ()
setPackageEntryPg entry =
  runPg $ \conn ->
    void $ PG.execute conn upsertPackageQuery (packageEntryRow entry)

setPackageEntriesPg :: (MonadIO m, WithPGConnectionPool env m) => [PackageEntry] -> m ()
setPackageEntriesPg [] = pure ()
setPackageEntriesPg entries =
  runPg $ \conn ->
    void $ PG.executeMany conn upsertPackageQuery (map packageEntryRow entries)

upsertPackageQuery :: PG.Query
upsertPackageQuery =
  "INSERT INTO packages \
  \(index_name, project, filename, url_token, upstream_url, expected_sha256, requires_python, core_metadata) \
  \VALUES (?,?,?,?,?,?,?,?) \
  \ON CONFLICT (index_name, url_token) DO UPDATE SET \
  \  upstream_url    = EXCLUDED.upstream_url, \
  \  expected_sha256 = COALESCE(EXCLUDED.expected_sha256, packages.expected_sha256), \
  \  requires_python = EXCLUDED.requires_python, \
  \  core_metadata   = COALESCE(EXCLUDED.core_metadata, packages.core_metadata), \
  \  updated_at      = now()"

packageEntryRow :: PackageEntry -> (IndexName, T.Text, T.Text, T.Text, T.Text, Maybe Sha256, Maybe T.Text, Maybe Sha256)
packageEntryRow entry =
  ( peIndexName entry,
    peProject entry,
    peFilename entry,
    peUrlToken entry,
    peUpstreamUrl entry,
    peExpectedSha256 entry,
    peRequiresPython entry,
    peCoreMetadata entry
  )

lookupCacheEntryPg :: (MonadIO m, WithPGConnectionPool env m) => IndexName -> T.Text -> m (Maybe CacheEntry)
lookupCacheEntryPg idx token =
  runPg $ \conn ->
    listToMaybe
      <$> PG.query
        conn
        "SELECT upstream_url, s3_key, metadata_s3_key \
        \FROM packages \
        \WHERE index_name = ? AND url_token = ?"
        (idx, token)

markPackageCachedPg ::
  (MonadIO m, WithPGConnectionPool env m) =>
  IndexName ->
  T.Text ->
  T.Text ->
  PackageKind ->
  Int64 ->
  m ()
markPackageCachedPg idx token keyS3 kind size =
  runPg $ \conn ->
    void $
      PG.execute conn query (keyS3, size, idx, token)
  where
    query = case kind of
      Package ->
        "UPDATE packages \
        \SET s3_key = ?, size = ?, updated_at = now() \
        \WHERE index_name = ? AND url_token = ?"
      Metadata ->
        "UPDATE packages \
        \SET metadata_s3_key = ?, metadata_size = ?, updated_at = now() \
        \WHERE index_name = ? AND url_token = ?"

cachedByIndexPg :: (MonadIO m, WithPGConnectionPool env m) => m (Map.Map IndexName Int)
cachedByIndexPg =
  runPg $ \conn -> do
    rows <-
      PG.query_
        conn
        "SELECT index_name, count(*)::int \
        \FROM packages \
        \WHERE s3_key IS NOT NULL \
        \GROUP BY index_name"
    pure $ Map.fromList rows

isProjectSyncPg ::
  (MonadIO m, WithPGConnectionPool env m) =>
  IndexName ->
  T.Text ->
  NominalDiffTime -> -- ttl
  m Bool
isProjectSyncPg idx project ttl =
  runPg $ \conn -> do
    rows <-
      PG.query
        conn
        "SELECT fetched_at > now() - make_interval(secs => ?) \
        \FROM index_fetches WHERE index_name = ? AND project = ?"
        (realToFrac ttl :: Double, idx, project)
    pure $ case rows of
      [Only sync] -> sync
      _ -> False

markProjectFetchedPg :: (MonadIO m, WithPGConnectionPool env m) => IndexName -> T.Text -> m ()
markProjectFetchedPg idx project =
  runPg $ \conn ->
    void $
      PG.execute
        conn
        "INSERT INTO index_fetches (index_name, project, fetched_at) \
        \VALUES (?, ?, now()) \
        \ON CONFLICT (index_name, project) DO UPDATE SET fetched_at = now()"
        (idx, project)

getProjectEntriesPg :: (MonadIO m, WithPGConnectionPool env m) => IndexName -> T.Text -> m [PackageEntry]
getProjectEntriesPg idx project =
  runPg $ \conn ->
    PG.query
      conn
      "SELECT index_name, project, filename, url_token, upstream_url, expected_sha256, requires_python, core_metadata \
      \FROM packages WHERE index_name = ? AND project = ? \
      \ORDER BY filename"
      (idx, project)
