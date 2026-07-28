module Prokki.Handlers.IndexesHandler (indexesHandler) where

import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Duration (humanReadableDuration)
import qualified Data.Map.Strict as Map
import qualified Data.OrdPSQ as PSQ
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, responseLBS)
import Prokki.Env (WithIndexes, WithPGConnectionPool, WithPackageCache, WithPackageCacheMaxSize, WithProjectCache, WithProjectCacheMaxSize, WithProkkiBaseUrl, WithRequestCounters, WithStartTime, grab)
import Prokki.Storage.Postgres (cachedByIndexPg)
import Prokki.Types.Config (PackageCacheMaxSize (..), ProjectCacheMaxSize (..), ProkkiBaseUrl (..))
import Prokki.Types.Domain (Index (..), IndexName (..), Indexes, PackageCache, ProjectCache, RequestCounters)
import Prokki.Utils (uriToText)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- | The handler renders table with proxied indexes.
indexesHandler ::
  ( MonadIO m,
    WithProkkiBaseUrl env m,
    WithIndexes env m,
    WithRequestCounters env m,
    WithStartTime env m,
    WithPGConnectionPool env m,
    WithProjectCache env m,
    WithProjectCacheMaxSize env m,
    WithPackageCache env m,
    WithPackageCacheMaxSize env m
  ) =>
  Request ->
  m Response
indexesHandler _ = do
  baseUrl <- grab @ProkkiBaseUrl
  indexes <- grab @Indexes
  requestCounters <- grab @(TVar RequestCounters) >>= liftIO . readTVarIO
  startTime <- grab @UTCTime
  indexesStat <- cachedByIndexPg
  nowTime <- liftIO getCurrentTime
  projectCache <- grab @(TVar ProjectCache) >>= liftIO . readTVarIO
  packageCache <- grab @(TVar PackageCache) >>= liftIO . readTVarIO
  ProjectCacheMaxSize projectCacheMaxSize <- grab @ProjectCacheMaxSize
  PackageCacheMaxSize packageCacheMaxSize <- grab @PackageCacheMaxSize
  let addr = uriToText (unProkkiBaseUrl baseUrl)
      uptime = diffUTCTime nowTime startTime
      projectCacheSize = PSQ.size projectCache
      packageCacheSize = PSQ.size packageCache
      htmlPage = renderHtml $ indexesPage addr indexes requestCounters uptime indexesStat projectCacheSize projectCacheMaxSize packageCacheSize packageCacheMaxSize
  pure $ responseLBS status200 [("Content-Type", "text/html")] htmlPage

indexesPage :: T.Text -> Indexes -> RequestCounters -> NominalDiffTime -> Map.Map IndexName Int -> Int -> Int -> Int -> Int -> H.Html
indexesPage addr indexes requestCounters uptime indexesStat projectCacheSize projectCacheMaxSize packageCacheSize packageCacheMaxSize = H.docTypeHtml $ do
  H.head $ do
    H.meta H.! A.charset "UTF-8"
    H.title "Prokki Indexes"
  H.body $ do
    H.h1 ("Proxied Indexes" <> H.toHtml ("\x1F996" :: String))
    H.table H.! A.style "border: 1px solid" $ do
      H.tr $ do
        H.th "Index"
        H.th "Proxy"
        H.th "Origin"
        H.th "Packages"
      mapM_ renderIndex indexes

    H.h1 "Statistics"
    H.p $ H.toHtml ("Uptime: " <> humanReadableDuration (realToFrac uptime))
    H.p $ H.toHtml ("Project cache (in-memory): " <> show projectCacheSize <> " / " <> show projectCacheMaxSize)
    H.p $ H.toHtml ("Package cache (in-memory): " <> show packageCacheSize <> " / " <> show packageCacheMaxSize)
    if Map.null requestCounters
      then mempty
      else do
        H.table H.! A.style "border: 1px solid" $ do
          H.tr $ do
            H.th "Handler"
            H.th "Requests"
          mapM_ renderStats (Map.toList requestCounters)
  where
    renderIndex :: Index -> H.Html
    renderIndex Index {..} = H.tr $ do
      let index' = addr <> "/" <> unIndexName indexName
          origin' = indexOrigin <> indexPath
          cached = Map.findWithDefault 0 indexName indexesStat
      H.td $ H.toHtml (unIndexName indexName)
      H.td $ H.a H.! A.href (H.toValue index') $ H.toHtml index'
      H.td $ H.a H.! A.href (H.toValue origin') $ H.toHtml origin'
      H.td $ H.toHtml (show cached)
    renderStats :: (T.Text, Int) -> H.Html
    renderStats (index, counter) = H.tr $ do
      H.td $ H.toHtml index
      H.td $ H.toHtml counter
