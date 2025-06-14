module Prokki.Handlers.IndexesHandler (indexesHandler) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, responseLBS)
import Prokki.Env (WithIndexes, grab)
import Prokki.Type (Index (..), Indexes)
import Prokki.Utils (remoteAddress)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html4.FrameSet.Attributes (href)
import qualified Text.Blaze.Html4.FrameSet.Attributes as A
import qualified Text.Blaze.Html5 as H

-- | The handler renders table with proxied indexes.
indexesHandler :: (MonadIO m, WithIndexes env m) => Request -> m Response
indexesHandler req = do
  indexes <- grab @Indexes
  let addr = remoteAddress req
      htmlPage = renderHtml $ indexesPage addr indexes
  pure $ responseLBS status200 [("Content-Type", "text/html")] htmlPage

indexesPage :: T.Text -> Indexes -> H.Html
indexesPage addr indexes = H.docTypeHtml $ do
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
  where
    renderIndex :: Index -> H.Html
    renderIndex Index {..} = H.tr $ do
      let index' = addr <> "/" <> index
          origin' = origin <> path
      H.td $ H.toHtml index
      H.td $ H.a H.! href (H.toValue index') $ H.toHtml index'
      H.td $ H.a H.! href (H.toValue origin') $ H.toHtml origin'
