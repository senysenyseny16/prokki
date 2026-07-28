module Prokki.Types.CacheEntry (CacheEntry (..)) where

import qualified Data.Text as T

data CacheEntry = CacheEntry
  { ceUpstreamUrl :: T.Text,
    ceS3Key :: Maybe T.Text,
    ceMetadataS3Key :: Maybe T.Text
  }
  deriving (Show, Eq)
