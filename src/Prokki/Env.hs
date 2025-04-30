{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Prokki.Env (Env (..), Has (..), grab) where

import Colog (HasLog (..), LogAction, Message)
import Control.Monad.Reader (MonadReader, asks)
import Network.HTTP.Conduit (Manager)
import Prokki.Type (Address, Cache, Index)

data Env m = Env
  { envAddress :: !Address,
    envIndex :: !Index,
    envCache :: !Cache,
    envManager :: !Manager,
    envLogAction :: !(LogAction m Message)
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

grab :: forall field env m. (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}
