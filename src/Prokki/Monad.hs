{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Prokki.Monad (Prokki, unProkki, ProkkiEnv, runProkki) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, liftResourceT)
import Prokki.Env (Env)

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

runProkki :: ProkkiEnv -> Prokki a -> IO a
runProkki env prokki = runReaderT (unProkki prokki) env
