{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prokki.Env
  ( Env (..),
    Has (..),
    grab,
    WithAddress,
    WithProkkiBaseUrl,
    WithProjectCacheTtl,
    WithIndexes,
    WithManager,
    WithSettings,
    WithRequestCounters,
    WithUploadRegistry,
    WithProjectCache,
    WithProjectCacheMaxSize,
    WithPackageCache,
    WithPackageCacheMaxSize,
    WithStartTime,
    WithResponseTimeout,
    WithPGConnectionPool,
    WithS3,
  )
where

import Colog (HasLog (..), LogAction, Message)
import Control.Concurrent.STM (TVar)
import Control.Monad.Reader (MonadReader, asks)
import Data.Time.Clock (UTCTime)
import Network.HTTP.Conduit (Manager)
import Prokki.Storage.Postgres.Types (PGConnectionPool)
import Prokki.Storage.S3.Types (S3Env)
import Prokki.Types.Config
import Prokki.Types.Domain
import Prokki.Types.Network

data Env m = Env
  { envAddress :: !Address,
    envProkkiBaseUrl :: !ProkkiBaseUrl,
    envProjectCacheTtl :: !ProjectCacheTtl,
    envIndexes :: !Indexes,
    envManager :: !Manager,
    envLogAction :: !(LogAction m Message),
    envRequestCounters :: !(TVar RequestCounters),
    envUploadRegistry :: !(TVar UploadRegistry),
    envProjectCache :: !(TVar ProjectCache),
    envProjectCacheMaxSize :: !ProjectCacheMaxSize,
    envPackageCache :: !(TVar PackageCache),
    envPackageCacheMaxSize :: !PackageCacheMaxSize,
    envStartTime :: !UTCTime,
    envResponseTimeout :: !ResponseTimeout,
    envPgPool :: !PGConnectionPool,
    envS3 :: !S3Env
  }

instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = envLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction newLogAction env = env {envLogAction = newLogAction}
  {-# INLINE setLogAction #-}

class Has field env where
  obtain :: env -> field

instance Has Address (Env m) where obtain = envAddress

instance Has ProkkiBaseUrl (Env m) where obtain = envProkkiBaseUrl

instance Has ProjectCacheTtl (Env m) where obtain = envProjectCacheTtl

instance Has Indexes (Env m) where obtain = envIndexes

instance Has Manager (Env m) where obtain = envManager

instance Has (TVar RequestCounters) (Env m) where obtain = envRequestCounters

instance Has (TVar UploadRegistry) (Env m) where obtain = envUploadRegistry

instance Has (TVar ProjectCache) (Env m) where obtain = envProjectCache

instance Has ProjectCacheMaxSize (Env m) where obtain = envProjectCacheMaxSize

instance Has (TVar PackageCache) (Env m) where obtain = envPackageCache

instance Has PackageCacheMaxSize (Env m) where obtain = envPackageCacheMaxSize

instance Has UTCTime (Env m) where obtain = envStartTime

instance Has ResponseTimeout (Env m) where obtain = envResponseTimeout

instance Has PGConnectionPool (Env m) where obtain = envPgPool

instance Has S3Env (Env m) where obtain = envS3

type WithAddress r m = (MonadReader r m, Has Address r)

type WithProkkiBaseUrl r m = (MonadReader r m, Has ProkkiBaseUrl r)

type WithProjectCacheTtl r m = (MonadReader r m, Has ProjectCacheTtl r)

type WithIndexes r m = (MonadReader r m, Has Indexes r)

type WithManager r m = (MonadReader r m, Has Manager r)

type WithRequestCounters r m = (MonadReader r m, Has (TVar RequestCounters) r)

type WithUploadRegistry r m = (MonadReader r m, Has (TVar UploadRegistry) r)

type WithProjectCache r m = (MonadReader r m, Has (TVar ProjectCache) r)

type WithProjectCacheMaxSize r m = (MonadReader r m, Has ProjectCacheMaxSize r)

type WithPackageCache r m = (MonadReader r m, Has (TVar PackageCache) r)

type WithPackageCacheMaxSize r m = (MonadReader r m, Has PackageCacheMaxSize r)

type WithStartTime r m = (MonadReader r m, Has UTCTime r)

type WithResponseTimeout r m = (MonadReader r m, Has ResponseTimeout r)

type WithPGConnectionPool r m = (MonadReader r m, Has PGConnectionPool r)

type WithS3 r m = (MonadReader r m, Has S3Env r)

type WithSettings r m =
  ( WithStartTime r m,
    WithAddress r m,
    WithProkkiBaseUrl r m,
    WithProjectCacheTtl r m,
    WithIndexes r m,
    WithProjectCacheMaxSize r m,
    WithPackageCacheMaxSize r m,
    WithResponseTimeout r m
  )

grab :: forall field env m. (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}
