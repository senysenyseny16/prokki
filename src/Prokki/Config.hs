module Prokki.Config (loadConfig) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import Network.URI (parseURI)
import Prokki.Types.Config
import Prokki.Types.Domain
import Prokki.Types.Network
import Prokki.Utils (splitUrlOriginPath)
import Toml ((.=))
import qualified Toml

loadConfig :: (MonadIO m) => FilePath -> m Config
loadConfig = Toml.decodeFile configCodec

configCodec :: Toml.TomlCodec Config
configCodec =
  Config
    <$> addressCodec .= address
    <*> Toml.map (Toml.dimap unIndexName IndexName (Toml.text "name")) indexCodec "index" .= indexes
    <*> Toml.read "log.severity" .= logSeverity
    <*> Toml.int "response_timeout" .= responseTimeout
    <*> projectCacheMaxSizeCodec "project_cache_max_size" .= projectCacheMaxSize
    <*> packageCacheMaxSizeCodec "package_cache_max_size" .= packageCacheMaxSize
    <*> Toml.table pgConfigCodec "postgres" .= pg
    <*> Toml.table s3ConfigCodec "s3" .= s3
    <*> Toml.table prokkiBaseUrlCodec "http" .= prokkiBaseUrl
    <*> projectCacheTtlCodec "project_cache_ttl" .= projectCacheTtl

addressCodec :: Toml.TomlCodec Address
addressCodec =
  Address
    <$> Toml.text "host" .= addressHost
    <*> Toml.int "port" .= addressPort

indexCodec :: Toml.TomlCodec Index
indexCodec = Toml.dimatch toTuple fromTuple (Toml.pair (Toml.text "name") (Toml.text "url"))
  where
    toTuple (Index index origin path) = Just (unIndexName index, origin <> path)
    fromTuple (index, url) =
      case splitUrlOriginPath url of
        Just (origin, path) -> Index {indexName = IndexName index, indexOrigin = origin, indexPath = path}
        Nothing -> error ("Invalid url: " ++ T.unpack url)

pgConfigCodec :: Toml.TomlCodec PGConfig
pgConfigCodec =
  PGConfig
    <$> Toml.text "host" .= pgHost
    <*> Toml.int "port" .= pgPort
    <*> Toml.text "database" .= pgDatabase
    <*> Toml.bool "secure" .= pgSecure

s3ConfigCodec :: Toml.TomlCodec S3Config
s3ConfigCodec =
  S3Config
    <$> Toml.text "host" .= s3Host
    <*> Toml.int "port" .= s3Port
    <*> Toml.text "bucket" .= s3Bucket
    <*> Toml.bool "secure" .= s3Secure

prokkiBaseUrlCodec :: Toml.TomlCodec ProkkiBaseUrl
prokkiBaseUrlCodec = Toml.dimatch toText parseProkkiBaseUrl (Toml.text "base_url")
  where
    toText :: ProkkiBaseUrl -> Maybe T.Text
    toText (ProkkiBaseUrl uri) = Just (T.pack (show uri))

parseProkkiBaseUrl :: T.Text -> ProkkiBaseUrl
parseProkkiBaseUrl text = case parseURI (T.unpack text) of
  Just url -> ProkkiBaseUrl url
  Nothing -> error "Cannot parse Prokki base URL"

projectCacheTtlCodec :: Toml.Key -> Toml.TomlCodec ProjectCacheTtl
projectCacheTtlCodec key =
  Toml.dimap toMin fromMin (Toml.int key)
  where
    toMin (ProjectCacheTtl ndt) = round (ndt / 60)
    fromMin mins = ProjectCacheTtl (fromIntegral mins * 60)

projectCacheMaxSizeCodec :: Toml.Key -> Toml.TomlCodec ProjectCacheMaxSize
projectCacheMaxSizeCodec key = Toml.dimap unProjectCacheMaxSize ProjectCacheMaxSize (Toml.int key)

packageCacheMaxSizeCodec :: Toml.Key -> Toml.TomlCodec PackageCacheMaxSize
packageCacheMaxSizeCodec key = Toml.dimap unPackageCacheMaxSize PackageCacheMaxSize (Toml.int key)
