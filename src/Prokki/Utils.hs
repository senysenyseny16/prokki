{-# LANGUAGE OverloadedStrings #-}

module Prokki.Utils (replacePackageLink, compress, getPath, prokkiVersion) where

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Functor (($>))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Version (showVersion)
import Data.Void (Void)
import Network.Wai (Request, pathInfo)
import Paths_prokki (version)
import Replace.Megaparsec (streamEdit)
import Text.Megaparsec (Parsec, anySingle, manyTill, try, (<|>))
import Text.Megaparsec.Char (string)

compress :: LBS8.ByteString -> LBS8.ByteString
compress = GZip.compressWith GZip.defaultCompressParams {GZip.compressLevel = GZip.bestCompression}

replacePackageLink :: LBS.ByteString -> T.Text -> LBS.ByteString
replacePackageLink input replacer =
  let text = (decodeUtf8 . LBS8.toStrict) input
      replaced = streamEdit packageLinkParser (const ("href=\"" <> replacer <> "/packages")) text
   in (LBS.fromStrict . encodeUtf8) replaced

hostParser :: Parsec Void T.Text String
hostParser = (try (string "https://") <|> string "http://") *> manyTill anySingle (string "/")

packageLinkParser :: Parsec Void T.Text ()
packageLinkParser = string "href=\"" *> hostParser *> string "packages" $> ()

getPath :: Request -> T.Text
getPath = T.intercalate "/" . pathInfo

prokkiVersion :: String
prokkiVersion = showVersion version
