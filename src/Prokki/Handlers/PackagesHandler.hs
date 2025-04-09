{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Prokki.Handlers.PackagesHandler (packagesHandler) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Monad.Trans.Resource (ResIO)
import Data.ByteString.Builder (byteString)
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.Combinators as CC
import qualified Network.HTTP.Conduit as C
import Network.Wai (Request, Response, responseStream)
import Prokki.Utils (getPath)

packagesHandler :: C.Manager -> Request -> ResIO Response
packagesHandler manager req = do
  let path = getPath req
      url = "https://pypi.org/" ++ path

  request <- liftIO $ C.parseRequest url
  response <- C.http request {C.decompress = const False} manager
  let status = C.responseStatus response
      headers = C.responseHeaders response
  withRunInIO \unlift ->
    pure $ responseStream status headers $ \write flush -> do
      let bodySource = C.responseBody response
      unlift $ runConduit $ bodySource .| CC.mapM_ (liftIO . write . byteString)
      flush
