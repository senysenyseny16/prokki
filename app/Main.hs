import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Prokki.RequestDispatcher (requestDispatcher)

prokki :: Application
prokki req respond = requestDispatcher req >>= respond

main :: IO ()
main = run 8080 prokki
