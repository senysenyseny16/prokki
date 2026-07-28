module Prokki.SimpleIndex.Utils (toPackageEntry, normalizeUrl, normalizeProject) where

import Data.Char (toLower)
import qualified Data.Text as T
import Network.URI
  ( URI (..),
    URIAuth (..),
    escapeURIString,
    isUnreserved,
    parseURI,
    unEscapeString,
    uriRegName,
    uriToString,
  )
import Prokki.Domain (hashUrl)
import Prokki.SimpleIndex.Types
import Prokki.Types.Domain (IndexName (..))
import Prokki.Types.PackageEntry (PackageEntry (..))

toPackageEntry :: IndexName -> T.Text -> PackageLink -> PackageEntry
toPackageEntry indexName project pkgLink =
  PackageEntry
    { peIndexName = indexName,
      peProject = project,
      peFilename = plFilename pkgLink,
      peUrlToken = hashUrl (normalizeUrl (plHref pkgLink)),
      peUpstreamUrl = plHref pkgLink,
      peExpectedSha256 = plSha256 pkgLink,
      peRequiresPython = plRequiresPython pkgLink,
      peCoreMetadata = plCoreMetadata pkgLink
    }

normalizeUrl :: T.Text -> T.Text
normalizeUrl absUrl =
  case parseURI (T.unpack absUrl) of
    Nothing -> absUrl
    Just uri ->
      let keep c = isUnreserved c || c == '/'
          path' = escapeURIString keep (unEscapeString (uriPath uri))
          auth' = fmap lowerHost (uriAuthority uri)
          lowerHost a = a {uriRegName = map toLower (uriRegName a)}
          uri' =
            uri
              { uriScheme = map toLower (uriScheme uri),
                uriAuthority = auth',
                uriPath = path',
                uriFragment = ""
              }
       in T.pack $ uriToString id uri' ""

normalizeProject :: T.Text -> T.Text
normalizeProject =
  T.toLower . collapseSeparators
  where
    collapseSeparators =
      T.intercalate "-" . filter (not . T.null) . T.split (`elem` ['-', '_', '.'])
