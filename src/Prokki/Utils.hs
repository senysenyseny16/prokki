module Prokki.Utils
  ( replacePackageLink,
    escapeUnreservedChars,
    getHost,
    prokkiVersion,
    tempExt,
    noCompressionTlsManagerSettings,
    packageExts,
    isPackage,
    parseUrl,
  )
where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Either (fromRight)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Version (showVersion)
import Data.Void (Void)
import Network.HTTP.Client.Conduit (managerModifyRequest)
import qualified Network.HTTP.Conduit as C
import Network.URI (URI (uriAuthority, uriPath, uriScheme), escapeURIString, isUnreserved, parseAbsoluteURI, uriRegName)
import Paths_prokki (version)
import Prokki.Type (PackageLinkType (..))
import Replace.Megaparsec (streamEdit)
import Text.Megaparsec (MonadParsec (takeWhileP), Parsec, parse, try, (<|>))
import Text.Megaparsec.Char (char, string)

noCompressionTlsManagerSettings :: C.ManagerSettings
noCompressionTlsManagerSettings = C.tlsManagerSettings {managerModifyRequest = \req -> return req {C.requestHeaders = newHeaders req}}
  where
    newHeaders req = ("Accept-Encoding", "identity") : filter ((/= "Accept-Encoding") . fst) (C.requestHeaders req)

escapeUnreservedChars :: T.Text -> T.Text
escapeUnreservedChars url = T.pack $ escapeURIString isUnreserved (T.unpack url)

replacePackageLink :: LBS.ByteString -> T.Text -> T.Text -> LBS.ByteString
replacePackageLink input host index =
  let text = (decodeUtf8 . LBS8.toStrict) input
      replacer link =
        case link of
          Absolute -> "href=\"" <> host <> "/" <> index <> "/"
          Relative -> "href=\"" <> "/" <> index <> "/"
      replaced = streamEdit packageLinkParser replacer text
   in (LBS.fromStrict . encodeUtf8) replaced

hostParser :: Parsec Void T.Text T.Text
hostParser = do
  scheme <- try (string "https://") <|> string "http://"
  host <- takeWhileP Nothing (/= '/')
  _ <- char '/'
  pure $ scheme <> host <> "/"

absoluteLinkParser :: Parsec Void T.Text PackageLinkType
absoluteLinkParser = do
  _ <- string "href=\""
  _ <- hostParser
  return Absolute

relativeLinkParser :: Parsec Void T.Text PackageLinkType
relativeLinkParser = do
  _ <- string "href=\"/"
  return Relative

packageLinkParser :: Parsec Void T.Text PackageLinkType
packageLinkParser = try absoluteLinkParser <|> relativeLinkParser

getHost :: T.Text -> T.Text
getHost url = fromRight "Cannot parse host" (parse hostParser "" url)

parseUrl :: T.Text -> Maybe (T.Text, T.Text)
parseUrl url = do
  uri <- parseAbsoluteURI (T.unpack url)
  auth <- uriAuthority uri
  let origin = T.pack $ uriScheme uri ++ "//" ++ uriRegName auth
      path = T.dropWhileEnd (== '/') $ T.pack (uriPath uri)
  return (origin, path)

tempExt :: String
tempExt = ".tmp"
{-# INLINE tempExt #-}

-- | File extensions considered for caching (packages).
packageExts :: [T.Text]
packageExts =
  [ ".whl", -- wheels
    ".gz", -- sources
    ".metadata", -- metadata
    ".asc" -- GPG signature
  ]
{-# INLINE packageExts #-}

isPackage :: [T.Text] -> Bool
isPackage [] = False
isPackage xs = any (`T.isSuffixOf` last xs) packageExts

prokkiVersion :: T.Text
prokkiVersion = T.pack $ showVersion version
