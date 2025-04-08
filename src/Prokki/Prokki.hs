module Prokki.Prokki (prokki) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Resource (runResourceT)
import qualified Network.HTTP.Conduit as C
import Network.Wai (Application)
import Prokki.RequestDispatcher (requestDispatcher)
import Prokki.Settings (Settings (..))

prokki :: Settings -> C.Manager -> Application
prokki settings manager req respond = do
  runResourceT $ requestDispatcher req manager settings >>= liftIO . respond
