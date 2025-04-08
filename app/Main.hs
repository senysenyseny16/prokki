import qualified Network.HTTP.Conduit as C
import Network.Wai.Handler.Warp (run)
import Prokki.Middleware.LogMiddleware (logMiddleware)
import Prokki.Prokki (prokki)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)

main :: IO ()
main = do
  manager <- C.newManager C.tlsManagerSettings
  loggerSet <- newStdoutLoggerSet defaultBufSize
  run 8080 $ logMiddleware loggerSet (prokki manager)
