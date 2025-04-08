{-# LANGUAGE OverloadedStrings #-}

module Prokki.Handlers.ErrorHandler (errorHandler) where

import Network.HTTP.Types (status404)
import Network.Wai (Request, Response, responseLBS)

errorHandler :: Request -> IO Response
errorHandler _ = return $ responseLBS status404 [("Content-Type", "text/plain")] "404 Not Found"
