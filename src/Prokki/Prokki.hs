module Prokki.Prokki (prokki) where

import qualified Network.HTTP.Conduit as C
import Network.Wai (Application)
import Prokki.RequestDispatcher (requestDispatcher)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.IO.Class (MonadIO(liftIO))

prokki :: C.Manager -> Application
prokki manager req respond = runResourceT $ requestDispatcher manager req >>= liftIO . respond
