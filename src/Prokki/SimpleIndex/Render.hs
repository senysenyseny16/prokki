module Prokki.SimpleIndex.Render (renderProjectPage, renderIndexRootPage) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Prokki.Domain (s3KeyFromToken)
import Prokki.SimpleIndex.Utils
import Prokki.Types.Config (ProkkiBaseUrl (..))
import Prokki.Types.Domain (IndexName (..), Sha256 (unSha256))
import Prokki.Types.PackageEntry
import Prokki.Utils (uriToText)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

renderProjectPage :: ProkkiBaseUrl -> IndexName -> T.Text -> [PackageEntry] -> TL.Text
renderProjectPage baseUrl indexName project entries =
  renderHtml $ H.docTypeHtml $ do
    H.head $ do
      H.title $ H.toHtml ("Links for " <> project)

    H.body $ do
      H.h1 $ H.toHtml ("Links for " <> project)
      mapM_ (renderPackageEntry baseUrl indexName) entries

renderIndexRootPage :: IndexName -> [T.Text] -> TL.Text
renderIndexRootPage idx projects =
  renderHtml $ H.docTypeHtml $ do
    H.head $ H.title "Simple index"
    H.body $ mapM_ projectLink projects
  where
    projectLink p = do
      let normalized = normalizeProject p
          url = projectUrl normalized
      H.a H.! A.href (H.toValue url) $ H.toHtml p
      H.br
    projectUrl p = unIndexName idx <> "/" <> p <> "/"

renderPackageEntry :: ProkkiBaseUrl -> IndexName -> PackageEntry -> H.Html
renderPackageEntry baseUrl indexName pe = do
  let base' = mkProkkiUrl baseUrl indexName pe
      url = base' <> maybe "" (\sha -> "#sha256=" <> unSha256 sha) (peExpectedSha256 pe)

      withRequiresPython link = maybe link (\rp -> link H.! H.dataAttribute "requires-python" (H.toValue rp)) (peRequiresPython pe)

      withCoreMetadata link =
        maybe
          link
          ( \sha ->
              let val = "sha256=" <> unSha256 sha
               in link
                    H.! H.dataAttribute "core-metadata" (H.toValue val)
                    H.! H.dataAttribute "dist-info-metadata" (H.toValue val)
          )
          (peCoreMetadata pe)

      link' = withCoreMetadata . withRequiresPython $ H.a H.! A.href (H.toValue url)

  link' $ H.toHtml (peFilename pe)
  H.br

mkProkkiUrl :: ProkkiBaseUrl -> IndexName -> PackageEntry -> T.Text
mkProkkiUrl baseUrl indexName pe =
  let token = peUrlToken pe
      indexName' = unIndexName indexName
      path = indexName' <> "/" <> s3KeyFromToken token <> "/" <> peFilename pe
   in ensureSlash (uriToText (unProkkiBaseUrl baseUrl)) <> path

ensureSlash :: T.Text -> T.Text
ensureSlash b = if "/" `T.isSuffixOf` b then b else b <> "/"
