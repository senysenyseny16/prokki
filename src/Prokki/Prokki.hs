module Prokki.Prokki (prokkiApp) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import Network.Wai (Application, Request, Response)
import Prokki.Env (Env)
import Prokki.Monad (Prokki (..), ProkkiEnv, runProkki)
import Prokki.RequestDispatcher (requestDispatcher)

prokkiApp :: ProkkiEnv -> Application
prokkiApp env req respond = do
  runProkki env (requestDispatcher req) >>= liftIO . respond

-- runProkkiM env requestDispatcher
-- runResourceT $ requestDispatcher req manager settings >>= liftIO . respond
