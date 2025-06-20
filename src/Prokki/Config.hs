module Prokki.Config (Config (..), loadConfig) where

import Colog (Severity)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import Prokki.Type (Address (..), Cache (..), Index (..), Indexes)
import Prokki.Utils (parseUrl)
import Toml ((.=))
import qualified Toml

data Config = Config
  { address :: !Address,
    cache :: !Cache,
    indexes :: !Indexes,
    logSeverity :: !Severity
  }

loadConfig :: (MonadIO m) => FilePath -> m Config
loadConfig = Toml.decodeFile configCodec

configCodec :: Toml.TomlCodec Config
configCodec =
  Config
    <$> addressCodec .= address
    <*> cacheCodec .= cache
    <*> Toml.map (Toml.text "name") indexCodec "index" .= indexes
    <*> Toml.read "log.severity" .= logSeverity

addressCodec :: Toml.TomlCodec Address
addressCodec =
  Address
    <$> Toml.text "host" .= host
    <*> Toml.int "port" .= port

cacheCodec :: Toml.TomlCodec Cache
cacheCodec = Cache <$> Toml.string "cache" .= cacheDir

indexCodec :: Toml.TomlCodec Index
indexCodec = Toml.dimatch toTuple fromTuple (Toml.pair (Toml.text "name") (Toml.text "url"))
  where
    toTuple (Index index origin path) = Just (index, origin <> path)
    fromTuple (index, url) =
      case parseUrl url of
        Just (origin, path) -> Index index origin path
        Nothing -> error ("Invalid url: " ++ T.unpack url)
