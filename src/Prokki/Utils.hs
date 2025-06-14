module Prokki.Utils
  ( noCompressionTlsManagerSettings,
    escapeUnreservedChars,
    remoteAddress,
    tempExt,
    packageExts,
    isPackage,
    hostParser,
    parseUrl,
    cleanTempFiles,
    countFiles,
    prokkiVersion,
  )
where

import Colog (Message, WithLog, log, pattern W)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Version (showVersion)
import Data.Void (Void)
import Network.HTTP.Client.Conduit (managerModifyRequest)
import qualified Network.HTTP.Conduit as C
import Network.URI (URI (uriAuthority, uriPath, uriScheme), escapeURIString, isUnreserved, parseAbsoluteURI, uriRegName)
import Network.Wai (Request, isSecure, requestHeaderHost, requestHeaders)
import Paths_prokki (version)
import System.Directory (removeFile)
import System.FilePath.Find (FileType (RegularFile), always, extension, fileType, find, (==?))
import Text.Megaparsec (MonadParsec (takeWhileP), Parsec, try, (<|>))
import Text.Megaparsec.Char (char, string)
import Prelude hiding (log)

noCompressionTlsManagerSettings :: C.ManagerSettings
noCompressionTlsManagerSettings = C.tlsManagerSettings {managerModifyRequest = \req -> return req {C.requestHeaders = newHeaders req}}
  where
    newHeaders req = ("Accept-Encoding", "identity") : filter ((/= "Accept-Encoding") . fst) (C.requestHeaders req)

escapeUnreservedChars :: T.Text -> T.Text
escapeUnreservedChars url = T.pack $ escapeURIString isUnreserved (T.unpack url)

remoteAddress :: Request -> T.Text
remoteAddress req = do
  let reqHeaders = requestHeaders req
      xfp = lookup "X-Forwarded-Proto" reqHeaders
      scheme = case xfp of
        Just proto -> decodeUtf8 proto
        Nothing -> if isSecure req then "https" else "http"
      host = fromMaybe (error "Host header now found") $ requestHeaderHost req
  scheme <> "://" <> decodeUtf8 host

hostParser :: Parsec Void T.Text T.Text
hostParser = do
  scheme <- try (string "https://") <|> string "http://"
  host <- takeWhileP Nothing (/= '/')
  _ <- char '/'
  pure $ scheme <> host <> "/"

parseUrl :: T.Text -> Maybe (T.Text, T.Text)
parseUrl url = do
  uri <- parseAbsoluteURI (T.unpack url)
  auth <- uriAuthority uri
  let origin = T.pack $ uriScheme uri ++ "//" ++ uriRegName auth
      path = T.dropWhileEnd (== '/') $ T.pack (uriPath uri)
  return (origin, path)

cleanTempFiles :: (MonadIO m, WithLog env Message m) => FilePath -> m ()
cleanTempFiles dir = do
  tempFiles <- liftIO $ find always (extension ==? tempExt) dir
  mapM_
    ( \file -> do
        log W $ "Removing temporary file: " <> T.pack file
        liftIO $ removeFile file
    )
    tempFiles

countFiles :: FilePath -> IO Int
countFiles dir = do
  files <- find always (fileType ==? RegularFile) dir
  return $ length files

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
