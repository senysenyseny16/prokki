module Prokki.Utils
  ( replacePackageLink,
    compress,
    getPath,
    prokkiVersion,
    tempExt,
    noCompressionTlsManagerSettings,
  )
where

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Functor (($>))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Version (showVersion)
import Data.Void (Void)
import Network.HTTP.Client.Conduit (managerModifyRequest)
import qualified Network.HTTP.Conduit as C
import Network.Wai (Request, pathInfo)
import Paths_prokki (version)
import Replace.Megaparsec (streamEdit)
import Text.Megaparsec (Parsec, anySingle, manyTill, try, (<|>))
import Text.Megaparsec.Char (string)

compress :: LBS8.ByteString -> LBS8.ByteString
compress = GZip.compressWith GZip.defaultCompressParams {GZip.compressLevel = GZip.bestCompression}

noCompressionTlsManagerSettings :: C.ManagerSettings
noCompressionTlsManagerSettings = C.tlsManagerSettings {managerModifyRequest = \req -> return req {C.requestHeaders = newHeaders req}}
  where
    newHeaders req = ("Accept-Encoding", "identity") : filter ((/= "Accept-Encoding") . fst) (C.requestHeaders req)

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
{-# INLINE getPath #-}

tempExt :: String
tempExt = ".tmp"
{-# INLINE tempExt #-}

prokkiVersion :: T.Text
prokkiVersion = T.pack $ showVersion version
