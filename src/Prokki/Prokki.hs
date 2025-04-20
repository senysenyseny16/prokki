module Prokki.Prokki (prokkiApp) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import Network.Wai (Application, Request, Response)
import Prokki.Env (Env)
import Prokki.Monad (ProkkiM (..))
import Prokki.RequestDispatcher (requestDispatcher)

mkProkkiApp :: Env -> (Request -> ProkkiM Response) -> Application
mkProkkiApp env handler req respond = do
  let action = runProkkiM $ handler req
  runResourceT $
    runStdoutLoggingT $
      runReaderT action env >>= liftIO . respond

prokkiApp :: Env -> Application
prokkiApp env = mkProkkiApp env requestDispatcher
