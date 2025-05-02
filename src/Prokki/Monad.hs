{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Prokki.Monad (Prokki, unProkki, ProkkiEnv, runProkki) where

import Conduit (MonadResource, ResourceT, runResourceT)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Prokki.Env (Env)

type ProkkiEnv = Env Prokki

newtype Prokki a = Prokki {unProkki :: ReaderT ProkkiEnv (ResourceT IO) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader ProkkiEnv,
      MonadThrow,
      MonadUnliftIO,
      MonadResource
    )

runProkki :: ProkkiEnv -> Prokki a -> IO a
runProkki env (Prokki action) = runResourceT $ runReaderT action env
