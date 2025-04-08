{-# LANGUAGE OverloadedStrings #-}

module Prokki.Middleware.LogMiddleware (logMiddleware) where

import qualified Data.ByteString.Char8 as BS
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Network.HTTP.Types (Status (statusCode))
import Network.Wai (Middleware, rawPathInfo, requestMethod, responseStatus)
import System.Log.FastLogger (LoggerSet, pushLogStrLn, toLogStr)

logMiddleware :: LoggerSet -> Middleware
logMiddleware loggerSet app req respond = do
  start <- getCurrentTime
  app req $ \res -> do
    end <- getCurrentTime
    let durationMs = floor (1000 * realToFrac (diffUTCTime end start) :: Double) :: Int
        method = requestMethod req
        path = rawPathInfo req
        status = statusCode (responseStatus res)

    timestamp <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp

    pushLogStrLn loggerSet $
      toLogStr $
        BS.concat
          [ "[",
            BS.pack timeStr,
            "] ",
            method,
            " ",
            path,
            " -> ",
            BS.pack (show status),
            " in ",
            BS.pack (show durationMs),
            "ms"
          ]

    respond res
