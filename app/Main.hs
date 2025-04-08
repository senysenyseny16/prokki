import qualified Network.HTTP.Conduit as C
import Network.Wai.Handler.Warp (run)
import Prokki.Prokki (prokki)

main :: IO ()
main = do
  manager <- C.newManager C.tlsManagerSettings
  run 8080 (prokki manager)
