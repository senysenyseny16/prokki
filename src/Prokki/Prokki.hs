module Prokki.Prokki (prokki) where

import qualified Network.HTTP.Conduit as C
import Network.Wai (Application)
import Prokki.RequestDispatcher (requestDispatcher)

prokki :: C.Manager -> Application
prokki manager req respond = do
  requestDispatcher manager req >>= respond
