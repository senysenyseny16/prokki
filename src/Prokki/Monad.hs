{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Prokki.Monad (ProkkiM, runProkkiM) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Trans.Resource (MonadResource, ResourceT)
import Prokki.Env (Env)

newtype ProkkiM a = ProkkiM {runProkkiM :: ReaderT Env (LoggingT (ResourceT IO)) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Env,
      MonadLogger,
      MonadResource,
      MonadThrow,
      MonadUnliftIO
    )
