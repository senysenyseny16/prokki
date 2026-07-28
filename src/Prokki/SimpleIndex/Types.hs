module Prokki.SimpleIndex.Types (PackageLink (..)) where

import qualified Data.Text as T
import Prokki.Types.Domain (Sha256)

data PackageLink = PackageLink
  { plFilename :: T.Text,
    plHref :: T.Text,
    plSha256 :: Maybe Sha256,
    plRequiresPython :: Maybe T.Text,
    plCoreMetadata :: Maybe Sha256
  }
  deriving (Show, Eq)
