{-# OPTIONS_GHC -Wno-orphans #-}

module Prokki.Storage.Postgres.Types (PGConnectionPool (..)) where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Prokki.Types.CacheEntry (CacheEntry (..))
import Prokki.Types.Domain (IndexName (..), Sha256 (..))
import Prokki.Types.PackageEntry (PackageEntry (..))

newtype PGConnectionPool = PGConnectionPool {unPGConnectionPool :: Pool Connection}

instance FromRow CacheEntry where
  fromRow =
    CacheEntry
      <$> field
      <*> field
      <*> field

instance FromRow PackageEntry where
  fromRow =
    PackageEntry
      <$> field -- index_name
      <*> field -- project
      <*> field -- filename
      <*> field -- url_token
      <*> field -- upstream_url
      <*> field -- expected package sha256
      <*> field -- requires python
      <*> field -- core metadata

instance FromField Sha256 where
  fromField f mbs = Sha256 <$> fromField f mbs

instance ToField Sha256 where
  toField = toField . unSha256

instance FromField IndexName where
  fromField f mbs = IndexName <$> fromField f mbs

instance ToField IndexName where
  toField = toField . unIndexName
