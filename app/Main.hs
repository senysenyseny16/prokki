import Colog (Msg (..), Severity (..), hoistLogAction, richMessageAction, (<&))
import Colog.Concurrent (defCapacity, withBackgroundLogger)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.Stack (HasCallStack, callStack)
import qualified Network.HTTP.Conduit as C
import Network.Wai.Handler.Warp (run)
import Options.Applicative (execParser, fullDesc, helper, info, progDesc, (<**>))
import Parser
import Prokki.Config (Config (..), loadConfig)
import Prokki.Env
import Prokki.Middleware.RequestLogger (logRequests)
import Prokki.Monad (ProkkiEnv)
import Prokki.Prokki (prokkiApp)
import Prokki.Type (Address (..))
import Prokki.Utils (noCompressionTlsManagerSettings, prokkiVersion)
import System.IO (BufferMode (..), hSetBuffering, stdout)

runProkki :: (HasCallStack) => Args -> IO ()
runProkki Args {..} = do
  hSetBuffering stdout LineBuffering
  Config {..} <- loadConfig configPath
  withBackgroundLogger defCapacity richMessageAction (pure ()) \logAction -> do
    cmanager <- C.newManager noCompressionTlsManagerSettings
    let prokkiEnv :: ProkkiEnv
        prokkiEnv =
          Env
            { envAddress = address,
              envIndexes = indexes,
              envCache = cache,
              envManager = cmanager,
              envLogAction = hoistLogAction liftIO logAction
            }
        prokkiInfo = "Prokki v" <> prokkiVersion <> " on " <> T.pack (show address)

    logAction <& (Msg {msgText = prokkiInfo, msgSeverity = Info, msgStack = callStack})
    logAction <& (Msg {msgText = T.pack (show cache), msgSeverity = Info, msgStack = callStack})
    let logIndex index = logAction <& (Msg {msgText = T.pack (show index), msgSeverity = Info, msgStack = callStack})
    mapM_ logIndex (M.elems indexes)

    run (port address) $ logRequests logAction (prokkiApp prokkiEnv)

main :: IO ()
main = runProkki =<< execParser opts
  where
    opts = info (argsParser <**> helper) (fullDesc <> progDesc "Prokki - Python packages index cache")
