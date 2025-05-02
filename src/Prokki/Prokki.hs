module Prokki.Prokki (prokkiApp) where

import Control.Monad.IO.Class (liftIO)
import Network.Wai (Application)
import Prokki.Monad (ProkkiEnv, runProkki)
import Prokki.RequestDispatcher (requestDispatcher)

prokkiApp :: ProkkiEnv -> Application
prokkiApp env req respond = runProkki env (requestDispatcher req >>= liftIO . respond)
