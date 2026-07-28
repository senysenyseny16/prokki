module Prokki.SimpleIndex.Parser
  ( parseProjectDocument,
    parseProjectPage,
    parseRootProjects,
    toPackageLink,
  )
where

import Control.Applicative ((<|>))
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.URI (parseURI, parseURIReference, relativeTo)
import Prokki.SimpleIndex.Types
import Prokki.Types.Domain (Sha256 (..))
import Text.HTML.DOM (parseLT)
import Text.XML (Document, Name)
import Text.XML.Cursor

parseProjectDocument :: T.Text -> Document -> [PackageLink]
parseProjectDocument baseUrl doc =
  let cursor = fromDocument doc
   in mapMaybe (toPackageLink baseUrl) (cursor $// element "a")

parseProjectPage :: T.Text -> TL.Text -> [PackageLink]
parseProjectPage baseUrl = parseProjectDocument baseUrl . parseLT

parseRootProjects :: TL.Text -> [T.Text]
parseRootProjects html =
  let cursor = fromDocument (parseLT html)
   in filter
        (not . T.null)
        [ T.strip (T.concat (a $/ content))
          | a <- cursor $// element "a"
        ]

toPackageLink :: T.Text -> Cursor -> Maybe PackageLink
toPackageLink baseUrl cursor = do
  href <- listToMaybe $ attribute "href" cursor
  let filename = T.strip . T.concat $ cursor $/ content
      (urlPart, frag) = T.breakOn "#sha256=" href
      absUrl = resolveUrlText baseUrl urlPart
      sha256 = parseSha "#sha256=" frag

      requiresPython = attrMaybe "data-requires-python" cursor
      coreMetadata =
        (attrMaybe "data-core-metadata" cursor <|> attrMaybe "data-dist-info-metadata" cursor)
          >>= parseSha "sha256="
  pure $ PackageLink filename absUrl sha256 requiresPython coreMetadata

resolveUrlText :: T.Text -> T.Text -> T.Text
resolveUrlText base url =
  case (parseURI (T.unpack base), parseURIReference (T.unpack url)) of
    (Just base', Just url') -> T.pack . show $ url' `relativeTo` base'
    _ -> url

attrMaybe :: Name -> Cursor -> Maybe T.Text
attrMaybe name cursor =
  case attribute name cursor of
    (v : _) | not (T.null v) -> Just v
    _ -> Nothing

parseSha :: T.Text -> T.Text -> Maybe Sha256
parseSha prefix v = Sha256 <$> T.stripPrefix prefix v
