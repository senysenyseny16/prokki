{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prokki.Env (Env (..), Has (..), grab, WithAddress, WithIndex, WithCache, WithManager, WithSettings) where

import Colog (HasLog (..), LogAction, Message)
import Control.Monad.Reader (MonadReader, asks)
import Network.HTTP.Conduit (Manager)
import Prokki.Type (Address, Cache, Index)
import qualified Control.Monad.Trans.Resource as Resource

data Env m = Env
  { envAddress :: !Address,
    envIndex :: !Index,
    envCache :: !Cache,
    envManager :: !Manager,
    envLogAction :: !(LogAction m Message),
    envReleaseMap :: Resource.InternalState
  }

instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = envLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction newLogAction env = env {envLogAction = newLogAction}
  {-# INLINE setLogAction #-}

class Has field env where
  obtain :: env -> field

instance Has Address (Env m) where obtain = envAddress

instance Has Index (Env m) where obtain = envIndex

instance Has Cache (Env m) where obtain = envCache

instance Has Manager (Env m) where obtain = envManager

type WithAddress r m = (MonadReader r m, Has Address r)

type WithIndex r m = (MonadReader r m, Has Index r)

type WithCache r m = (MonadReader r m, Has Cache r)

type WithManager r m = (MonadReader r m, Has Manager r)

type WithSettings r m = (WithAddress r m, WithIndex r m, WithCache r m)

grab :: forall field env m. (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}
