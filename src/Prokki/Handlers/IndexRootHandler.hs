module Prokki.Handlers.IndexRootHandler (indexRootHandler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Simple as HS
import Network.HTTP.Types (status200)
import Network.Wai (Response, responseLBS)
import Prokki.Domain (indexUpstreamUrl)
import Prokki.Env (WithManager, grab)
import Prokki.SimpleIndex.Parser (parseRootProjects)
import Prokki.SimpleIndex.Render (renderIndexRootPage)
import Prokki.Types.Domain (Index (..))

indexRootHandler :: (MonadIO m, WithManager env m) => Index -> m Response
indexRootHandler index = do
  let upstream = indexUpstreamUrl index
  body <- fetchIndexRoot upstream
  let projects = parseRootProjects body
  pure $
    responseLBS
      status200
      [("Content-Type", "text/html; charset=utf-8")]
      (TLE.encodeUtf8 (renderIndexRootPage (indexName index) projects))

fetchIndexRoot :: (MonadIO m, WithManager env m) => T.Text -> m TL.Text
fetchIndexRoot url = do
  manager <- grab @HC.Manager
  liftIO $ do
    req <- HS.parseRequest (T.unpack url)
    resp <- HS.httpLBS (HS.setRequestManager manager req)
    pure $ TLE.decodeUtf8 (HS.getResponseBody resp)
