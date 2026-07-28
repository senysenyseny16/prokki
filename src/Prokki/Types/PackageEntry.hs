module Prokki.Types.PackageEntry (PackageEntry (..), PackageKind (..)) where

import qualified Data.Text as T
import Prokki.Types.Domain (IndexName, Sha256)

data PackageKind = Package | Metadata
  deriving (Show, Eq)

data PackageEntry = PackageEntry
  { peIndexName :: IndexName,
    peProject :: T.Text,
    peFilename :: T.Text,
    peUrlToken :: T.Text,
    peUpstreamUrl :: T.Text,
    peExpectedSha256 :: Maybe Sha256,
    peRequiresPython :: Maybe T.Text,
    peCoreMetadata :: Maybe Sha256
  }
  deriving (Show, Eq)
