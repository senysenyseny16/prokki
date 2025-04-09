module Prokki.Prokki (prokki) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Resource (runResourceT)
import qualified Network.HTTP.Conduit as C
import Network.Wai (Application)
import Prokki.RequestDispatcher (requestDispatcher)

prokki :: C.Manager -> Application
prokki manager req respond = runResourceT $ requestDispatcher manager req >>= liftIO . respond
