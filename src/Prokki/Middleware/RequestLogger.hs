{-# LANGUAGE ScopedTypeVariables #-}

module Prokki.Middleware.RequestLogger (logRequests) where

import Colog (LogAction, Message, Msg (..), Severity (..), (<&))
import Control.Applicative ((<|>))
import Control.Exception (SomeException (..), throwIO, try)
import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import GHC.Stack (HasCallStack, callStack)
import Network.HTTP.Types (statusCode)
import Network.Wai (Middleware, Request, Response, httpVersion, rawPathInfo, remoteHost, requestHeaders, requestMethod, responseStatus)

logRequests :: (HasCallStack) => LogAction IO Message -> Middleware
logRequests logAction app req respond = do
  result <- try $ app req respond'
  case result of
    Left (exc :: SomeException) -> do
      logAction <& Msg {msgText = T.pack (show exc), msgSeverity = Error, msgStack = callStack}
      throwIO exc
    Right value -> pure value
  where
    respond' res = do
      let status = statusCode (responseStatus res)
          severity = if status == 200 || status == 301 || status == 302 then Info else Warning
      logAction <& (Msg {msgText = requestMsg req res, msgSeverity = severity, msgStack = callStack})
      respond res

requestMsg :: Request -> Response -> T.Text
requestMsg req res =
  let method = decodeUtf8 (requestMethod req)
      path = decodeUtf8 (rawPathInfo req)
      host = getClientIp req
      status = T.pack $ show (statusCode (responseStatus res))
      httpVer = T.pack $ show (httpVersion req)
   in T.intercalate " " [host, method, path, httpVer, status]

getClientIp :: Request -> T.Text
getClientIp req =
  let headers = requestHeaders req
      forwardedFor = decodeUtf8 <$> lookup "X-Forwarded-For" headers
      realIp = decodeUtf8 <$> lookup "X-Real-IP" headers
      hostIp = stripPort $ T.pack $ show (remoteHost req)
   in fromMaybe hostIp $ (forwardedFor >>= getFirstIp) <|> realIp

getFirstIp :: T.Text -> Maybe T.Text
getFirstIp txt = (find (not . T.null) . map T.strip) $ T.splitOn "," txt
{-# INLINE getFirstIp #-}

stripPort :: T.Text -> T.Text
stripPort = T.takeWhile (/= ':')
{-# INLINE stripPort #-}
