{-# LANGUAGE OverloadedStrings #-}

module Prokki.Handlers.PackagesHandler (packagesHandler) where

import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString.Builder (byteString)
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.Combinators as DCC
import qualified Network.HTTP.Conduit as C
import Network.Wai (Request, Response, responseStream)
import Prokki.Utils (getPath)

packagesHandler :: C.Manager -> Request -> IO Response
packagesHandler manager req = runResourceT $ do
  let path = getPath req
      url = "https://pypi.org/" ++ path

  request <- C.parseRequest url
  response <- C.http request manager
  let status = C.responseStatus response
      headers = C.responseHeaders response
  return $ responseStream status headers $ \write flush -> do
    let bodySource = C.responseBody response
    runConduit $ bodySource .| DCC.mapM_ (\chunk -> write (byteString chunk) >> flush)
