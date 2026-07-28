{-# LANGUAGE TemplateHaskell #-}

module Prokki.Utils
  ( uriToText,
    packageExts,
    splitUrlOriginPath,
    prokkiVersion,
    favicon,
  )
where

import qualified Data.ByteString.Lazy as BSL
import Data.FileEmbed (embedFile)
import qualified Data.Text as T
import Data.Version (showVersion)
import Network.URI (URI (uriAuthority, uriPath, uriScheme), parseAbsoluteURI, uriRegName, uriToString)
import Paths_prokki (version)
import Prelude hiding (log)

uriToText :: URI -> T.Text
uriToText uri = T.pack (uriToString id uri "")

splitUrlOriginPath :: T.Text -> Maybe (T.Text, T.Text)
splitUrlOriginPath url = do
  uri <- parseAbsoluteURI (T.unpack url)
  auth <- uriAuthority uri
  let origin = T.pack $ uriScheme uri ++ "//" ++ uriRegName auth
      path = T.dropWhileEnd (== '/') $ T.pack (uriPath uri)
  pure (origin, path)

-- | File extensions considered for caching (packages).
packageExts :: [T.Text]
packageExts =
  [ ".whl", -- wheels
    ".gz", -- sources
    ".zip", -- sources
    ".metadata", -- metadata
    ".asc", -- GPG signature
    ".exe"
  ]
{-# INLINE packageExts #-}

prokkiVersion :: T.Text
prokkiVersion = T.pack $ showVersion version

favicon :: BSL.ByteString
favicon = BSL.fromStrict $(embedFile "static/favicon.ico")
