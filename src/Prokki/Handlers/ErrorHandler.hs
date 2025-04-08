{-# LANGUAGE OverloadedStrings #-}

module Prokki.Handlers.ErrorHandler (errorHandler) where

import qualified Network.HTTP.Conduit as C
import Network.HTTP.Types (status404)
import Network.Wai (Request, Response, responseLBS)

errorHandler :: C.Manager -> Request -> IO Response
errorHandler _ _ = return $ responseLBS status404 [("Content-Type", "text/plain")] "404 Not Found"
