module Prokki.Types.Config
  ( ResponseTimeout (..),
    ProjectCacheMaxSize (..),
    PackageCacheMaxSize (..),
    ProkkiBaseUrl (..),
    ProjectCacheTtl (..),
    PGConfig (..),
    S3Config (..),
    Config (..),
  )
where

import Colog (Severity)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime)
import Network.URI (URI)
import Prokki.Types.Domain
import Prokki.Types.Network

newtype ResponseTimeout = ResponseTimeout Int
  deriving (Show)

newtype ProjectCacheMaxSize = ProjectCacheMaxSize {unProjectCacheMaxSize :: Int}
  deriving (Show)

newtype PackageCacheMaxSize = PackageCacheMaxSize {unPackageCacheMaxSize :: Int}

newtype ProkkiBaseUrl = ProkkiBaseUrl {unProkkiBaseUrl :: URI}
  deriving (Show)

newtype ProjectCacheTtl = ProjectCacheTtl NominalDiffTime
  deriving (Show, Eq)

data PGConfig = PGConfig
  { pgHost :: !T.Text,
    pgPort :: !Int,
    pgDatabase :: !T.Text
  }

data S3Config = S3Config
  { s3Host :: !T.Text,
    s3Port :: !Int,
    s3Bucket :: !T.Text
  }

data Config = Config
  { address :: !Address,
    indexes :: !Indexes,
    logSeverity :: !Severity,
    responseTimeout :: !Int,
    projectCacheMaxSize :: !ProjectCacheMaxSize,
    packageCacheMaxSize :: !PackageCacheMaxSize,
    pg :: !PGConfig,
    s3 :: !S3Config,
    prokkiBaseUrl :: !ProkkiBaseUrl,
    projectCacheTtl :: !ProjectCacheTtl
  }
