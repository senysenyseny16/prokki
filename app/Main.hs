import Colog (hoistLogAction, log, richMessageAction, usingLoggerT, pattern I)
import Colog.Concurrent (defCapacity, withBackgroundLogger)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import qualified Network.HTTP.Conduit as C
import Network.Wai.Handler.Warp (run)
import Options.Applicative (execParser, fullDesc, helper, info, progDesc, (<**>))
import Parser
import Prokki.Config (Config (..), loadConfig)
import Prokki.Env
import Prokki.Middleware.RequestLogger (logRequests)
import Prokki.Monad (ProkkiEnv)
import Prokki.Prokki (prokkiApp)
import Prokki.Type (Address (..), Cache (..))
import Prokki.Utils (cleanTempFiles, countFiles, noCompressionTlsManagerSettings, prokkiVersion)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Prelude hiding (log)

runProkki :: (HasCallStack) => Args -> IO ()
runProkki Args {..} = do
  hSetBuffering stdout LineBuffering
  Config {..} <- loadConfig configPath
  withBackgroundLogger defCapacity richMessageAction (pure ()) \logAction -> do
    usingLoggerT logAction do
      cleanTempFiles (cacheDir cache)

      log I $ "Prokki v" <> prokkiVersion <> " on " <> T.pack (show address)
      log I $ T.pack (show cache)
      mapM_ (log I . T.pack . show) (M.elems indexes)
      cachedPkgs <- liftIO $ countFiles (cacheDir cache)
      log I $ "Total packages in cache: " <> T.pack (show cachedPkgs)

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

    run (port address) $ logRequests logAction (prokkiApp prokkiEnv)

main :: IO ()
main = runProkki =<< execParser opts
  where
    opts = info (argsParser <**> helper) (fullDesc <> progDesc "Prokki - Python packages index cache")
