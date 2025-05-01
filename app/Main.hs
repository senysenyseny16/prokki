import Colog (hoistLogAction, richMessageAction, Msg (..), Severity (..), (<&))
import Colog.Concurrent (defCapacity, withBackgroundLogger)
import Control.Monad.IO.Class (liftIO)
import qualified Network.HTTP.Conduit as C
import Network.Wai.Handler.Warp (run)
import Options.Applicative (execParser, fullDesc, helper, info, progDesc, (<**>))
import Parser
import qualified Data.Text as T
import Prokki.Env
import Prokki.Middleware.RequestLogger (logRequests)
import Prokki.Monad (ProkkiEnv)
import GHC.Stack (callStack, HasCallStack)
import Prokki.Prokki (prokkiApp)
import Prokki.Type (Address (..))
import Prokki.Utils (noCompressionTlsManagerSettings, prokkiVersion)

runProkki :: HasCallStack => Args -> IO ()
runProkki Args {..} = do
  withBackgroundLogger defCapacity richMessageAction (pure ()) \logAction -> do
    cmanager <- C.newManager noCompressionTlsManagerSettings
    let prokkiEnv :: ProkkiEnv
        prokkiEnv =
          Env
            { envAddress = address,
              envIndex = index,
              envCache = cache,
              envManager = cmanager,
              envLogAction = hoistLogAction liftIO logAction
            }
        prokkiInfo = "Prokki v" <> prokkiVersion <> " on " <> T.pack (show address)

    logAction <& (Msg {msgText = prokkiInfo, msgSeverity = Info, msgStack = callStack})
    logAction <& (Msg {msgText = T.pack (show index), msgSeverity = Info, msgStack = callStack})
    logAction <& (Msg {msgText = T.pack (show cache), msgSeverity = Info, msgStack = callStack})

    run (port address) $ logRequests logAction (prokkiApp prokkiEnv)

main :: IO ()
main = runProkki =<< execParser opts
  where
    opts = info (argsParser <**> helper) (fullDesc <> progDesc "Prokki - Python packages index cache")
