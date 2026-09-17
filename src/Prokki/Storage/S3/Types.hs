module Prokki.Storage.S3.Types (S3Env (..), StreamError (..)) where

import qualified Amazonka as AWS
import Control.Exception (Exception)
import qualified Data.Text as T
import Network.HTTP.Types.Status (Status)

data StreamError
  = MissingContentLength
  | UnexpectedUpstreamStatus Status
  deriving (Show)

instance Exception StreamError

data S3Env = S3Env {s3AwsEnv :: AWS.Env, s3Bucket :: T.Text}
