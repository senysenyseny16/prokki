module Prokki.Storage.S3.Stream (streamPackageToS3) where

import qualified Amazonka as AWS
import qualified Amazonka.S3 as S3
import Control.Exception (throwIO)
import Control.Monad.IO.Unlift (MonadIO (liftIO))
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Char8 as BS8
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Network.HTTP.Conduit as HC
import Network.HTTP.Simple (ResponseHeaders)
import Network.HTTP.Types.Header (hContentLength)
import Prokki.Env (WithManager, WithS3, grab)
import Prokki.Storage.S3 (runS3)
import Prokki.Storage.S3.Types
import Text.Read (readMaybe)
import Prelude hiding (log)

streamPackageToS3 ::
  (MonadIO m, WithManager env m, WithS3 env m) =>
  T.Text ->
  T.Text ->
  m Int64
streamPackageToS3 url keyS3 = do
  manager <- grab @HC.Manager
  runS3 $ \awsEnv bucket -> do
    req <- withoutCompression <$> HC.parseRequest (T.unpack url)
    runResourceT $ do
      resp <- HC.http req manager
      len <- requireContentLength (HC.responseHeaders resp)
      let src = HC.responseBody resp
          chunked = AWS.ChunkedBody AWS.defaultChunkSize len src
          putReq =
            S3.newPutObject
              (S3.BucketName bucket)
              (S3.ObjectKey keyS3)
              (AWS.Chunked chunked)
      _ <- AWS.send awsEnv putReq
      pure (fromIntegral len :: Int64)

requireContentLength :: (MonadIO m) => ResponseHeaders -> m Integer
requireContentLength hdrs =
  case lookup hContentLength hdrs >>= (readMaybe . BS8.unpack) of
    Just n | n >= 0 -> pure n
    _ -> liftIO (throwIO MissingContentLength)

withoutCompression :: HC.Request -> HC.Request
withoutCompression req =
  req
    { HC.requestHeaders =
        ("Accept-Encoding", "identity")
          : filter ((/= "Accept-Encoding") . fst) (HC.requestHeaders req),
      HC.decompress = const False
    }
