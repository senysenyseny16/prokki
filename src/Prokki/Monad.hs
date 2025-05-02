{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Prokki.Monad (Prokki, unProkki, ProkkiEnv, runProkki) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT, runReaderT)
import Prokki.Env (Env(..))
import Control.Monad.Trans.Resource (MonadResource(..), runInternalState)

type ProkkiEnv = Env Prokki

newtype Prokki a = Prokki {unProkki :: ReaderT ProkkiEnv IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader ProkkiEnv,
      MonadThrow,
      MonadUnliftIO
    )

instance MonadResource Prokki where
  liftResourceT resource = do
    Env {envReleaseMap} <- ask
    liftIO $ runInternalState resource envReleaseMap

runProkki :: ProkkiEnv -> Prokki a -> IO a
runProkki env (Prokki action) = runReaderT action env
