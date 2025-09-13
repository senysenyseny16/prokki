module Prokki.Handlers.IndexesHandler (indexesHandler) where

import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Map.Strict as SM
import qualified Data.Text as T
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, responseLBS)
import Prokki.Env (WithIndexes, WithRequestCounters, grab)
import Prokki.Type (Index (..), Indexes, RequestCounters)
import Prokki.Utils (remoteAddress)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html4.FrameSet.Attributes (href)
import qualified Text.Blaze.Html4.FrameSet.Attributes as A
import qualified Text.Blaze.Html5 as H

-- | The handler renders table with proxied indexes.
indexesHandler :: (MonadIO m, WithIndexes env m, WithRequestCounters env m) => Request -> m Response
indexesHandler req = do
  indexes <- grab @Indexes
  requestCounters <- grab @(TVar RequestCounters) >>= liftIO . readTVarIO
  let addr = remoteAddress req
      htmlPage = renderHtml $ indexesPage addr indexes requestCounters
  pure $ responseLBS status200 [("Content-Type", "text/html")] htmlPage

indexesPage :: T.Text -> Indexes -> RequestCounters -> H.Html
indexesPage addr indexes requestCounters = H.docTypeHtml $ do
  H.head $ do
    H.title "Prokki Indexes"
  H.body $ do
    H.h1 "Proxied Indexes"
    H.table H.! A.border "1" $ do
      H.tr $ do
        H.th "Index"
        H.th "Proxy"
        H.th "Origin"
      mapM_ renderIndex indexes

    if SM.null requestCounters
      then mempty
      else do
        H.h1 "Statistics"
        H.table H.! A.border "1" $ do
          H.tr $ do
            H.th "Handler"
            H.th "Requests"
          mapM_ renderStats (SM.toList requestCounters)
  where
    renderIndex :: Index -> H.Html
    renderIndex Index {..} = H.tr $ do
      let index' = addr <> "/" <> index
          origin' = origin <> path
      H.td $ H.toHtml index
      H.td $ H.a H.! href (H.toValue index') $ H.toHtml index'
      H.td $ H.a H.! href (H.toValue origin') $ H.toHtml origin'
    renderStats :: (T.Text, Int) -> H.Html
    renderStats (index, counter) = H.tr $ do
      H.td $ H.toHtml index
      H.td $ H.toHtml counter
