module Prokki.Domain
  ( s3KeyFromToken,
    hashUrl,
    indexUpstreamUrl,
    packageKeyS3,
    packageUpstream,
    packageCache,
  )
where

import Crypto.Hash (hashWith)
import Crypto.Hash.Algorithms (SHA256 (..))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Prokki.Types.CacheEntry (CacheEntry (..))
import Prokki.Types.Domain (Index (..))
import Prokki.Types.PackageEntry (PackageKind (..))

-- | Build the S3 object key for a cached package file from its URL token.
-- "9eca590c8cc947825b8cd8e6f2e151f7" -> "packages/9e/ca/9eca590c8cc947825b8cd8e6f2e151f7"
s3KeyFromToken :: T.Text -> T.Text
s3KeyFromToken token =
  T.intercalate
    "/"
    [ "packages",
      T.take 2 token,
      T.take 2 (T.drop 2 token),
      token
    ]

hashUrl :: T.Text -> T.Text
hashUrl url = T.take 32 . T.pack . show $ hashWith SHA256 (encodeUtf8 url)

indexUpstreamUrl :: Index -> T.Text
indexUpstreamUrl Index {..} = indexOrigin <> indexPath <> "/"

packageKeyS3 :: PackageKind -> T.Text -> T.Text
packageKeyS3 Package token = s3KeyFromToken token
packageKeyS3 Metadata token = s3KeyFromToken token <> ".metadata"

packageUpstream :: PackageKind -> T.Text -> T.Text
packageUpstream Package = id
packageUpstream Metadata = (<> ".metadata")

packageCache :: PackageKind -> CacheEntry -> Maybe T.Text
packageCache Package = ceS3Key
packageCache Metadata = ceMetadataS3Key
