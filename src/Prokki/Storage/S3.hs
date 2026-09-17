module Prokki.Storage.S3 (mkS3Env, runS3, presignObjectS3, putObjectS3) where

import qualified Amazonka as AWS
import qualified Amazonka.S3 as S3
import qualified Amazonka.S3.GetObject as S3
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Lens.Micro ((&), (?~))
import Prokki.Env (WithS3, grab)
import Prokki.Storage.S3.Types (S3Env (..))

mkS3Env :: T.Text -> Int -> T.Text -> Bool -> IO S3Env
mkS3Env host port bucket secure = do
  env <- AWS.newEnv AWS.discover

  let overrideS3 svc =
        let svc' = AWS.setEndpoint secure (encodeUtf8 host) port svc
         in svc' {AWS.s3AddressingStyle = AWS.S3AddressingStylePath}
      env' = AWS.overrideService overrideS3 env

  pure $ S3Env env' bucket

runS3 :: (MonadIO m, WithS3 env m) => (AWS.Env -> T.Text -> IO a) -> m a
runS3 action = do
  s3 <- grab @S3Env
  liftIO $ action (s3AwsEnv s3) (s3Bucket s3)

presignObjectS3 :: (MonadIO m, WithS3 env m) => T.Text -> T.Text -> m ByteString
presignObjectS3 key filename =
  runS3 $ \awsEnv bucket -> presignUrl awsEnv bucket key filename

putObjectS3 :: (MonadIO m, WithS3 env m) => T.Text -> ByteString -> m ()
putObjectS3 key body =
  runS3 $ \awsEnv bucket -> do
    let request = S3.newPutObject (S3.BucketName bucket) (S3.ObjectKey key) (AWS.toBody body)
    runResourceT $ void (AWS.send awsEnv request)

presignUrl :: AWS.Env -> T.Text -> T.Text -> T.Text -> IO ByteString
presignUrl env bucket key filename = do
  now <- getCurrentTime
  let disposition = "attachment; filename=\"" <> filename <> "\""
      req =
        S3.newGetObject (S3.BucketName bucket) (S3.ObjectKey key)
          & S3.getObject_responseContentDisposition
            ?~ disposition
      expire = 60 * 30 -- 30 minutes
  AWS.presignURL env now expire req
