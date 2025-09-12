{-# LANGUAGE ScopedTypeVariables #-}

module Prokki.Handlers.IndexHandler (indexHandler) where

import Colog (Message, WithLog, log, pattern E)
import qualified Control.Exception as E
import Control.Monad.Catch (MonadCatch, catch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Void (Void)
import Network.HTTP.Conduit (Manager)
import qualified Network.HTTP.Conduit as C
import Network.HTTP.Types (hContentLength, status504)
import Network.Wai (Request, Response, responseLBS)
import Prokki.Env (WithManager, grab)
import Prokki.Type (Index (..), PackageLinkType (..), Path)
import Prokki.Utils (hostParser, remoteAddress)
import Replace.Megaparsec (streamEdit)
import Text.Megaparsec (Parsec, try, (<|>))
import Text.Megaparsec.Char (string)
import Prelude hiding (log)

-- | The handler fully replicates the "simple repository API",
-- replacing links from the original repository with its own.
-- Responses are not cached to always return the most up-to-date list of packages/versions.
-- https://packaging.python.org/en/latest/specifications/simple-repository-api/#base-html-api
indexHandler :: (MonadIO m, MonadCatch m, WithManager env m, WithLog env Message m) => Request -> Index -> Path -> m Response
indexHandler req Index {..} reqPath = do
  manager <- grab @Manager
  let url = origin <> path <> "/" <> T.intercalate "/" reqPath
      addr = remoteAddress req

  request <- liftIO $ C.parseRequest (T.unpack url)
  catch
    do
      response <- C.httpLbs request manager
      let headers = C.responseHeaders response
          newBody = replacePackageLink (C.responseBody response) addr index
          bodyLength = LBS.length newBody
          newHeaders = (hContentLength, BS.pack $ show bodyLength) : filter (\(h, _) -> h /= hContentLength) headers

      pure $ responseLBS (C.responseStatus response) newHeaders newBody
    ( \(e :: E.SomeException) -> do
        log E $ "Failed to download: " <> url <> ", error: " <> T.pack (show e)
        pure $ responseLBS status504 [("Content-Type", "text/plain")] "Upstream request failed"
    )

replacePackageLink :: LBS.ByteString -> T.Text -> T.Text -> LBS.ByteString
replacePackageLink input host index =
  let text = (decodeUtf8 . LBS8.toStrict) input
      replacer link =
        case link of
          Absolute -> "href=\"" <> host <> "/" <> index <> "/"
          Relative -> "href=\"" <> "/" <> index <> "/"
      replaced = streamEdit packageLinkParser replacer text
   in (LBS.fromStrict . encodeUtf8) replaced

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
