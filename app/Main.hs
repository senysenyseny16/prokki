import Colog (Msg (..), filterBySeverity, hoistLogAction, log, msgSeverity, richMessageAction, usingLoggerT, pattern I)
import Colog.Concurrent (defCapacity, withBackgroundLogger)
import Control.Concurrent.STM (newTVarIO)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import qualified Data.OrdPSQ as PSQ
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import GHC.Stack (HasCallStack)
import qualified Network.HTTP.Conduit as C
import Network.URI (uriToString)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (defaultGzipSettings, gzip)
import Options.Applicative
import Prokki.App (prokkiApp)
import Prokki.Config (loadConfig)
import Prokki.Env
import Prokki.Middleware.RequestLogger (logRequests)
import Prokki.Monad (ProkkiEnv)
import Prokki.Storage.Postgres (mkPgPool)
import Prokki.Storage.S3 (mkS3Env)
import Prokki.Types.Config (Config (..), PGConfig (..), ProjectCacheTtl (..), ProkkiBaseUrl (..), ResponseTimeout (..), S3Config (..))
import Prokki.Types.Domain (Index (..), IndexName (..))
import Prokki.Types.Network (Address (..))
import Prokki.Utils (prokkiVersion)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Prelude hiding (log)

newtype Args = Args {configPath :: FilePath}

runApp :: (HasCallStack) => Args -> IO ()
runApp Args {..} = do
  hSetBuffering stdout LineBuffering
  Config {..} <- loadConfig configPath
  withBackgroundLogger defCapacity richMessageAction (pure ()) \logAction -> do
    usingLoggerT logAction do
      log I $ "Prokki v" <> prokkiVersion <> " on " <> renderAddress address
      log I $ "Base URL: " <> T.pack (uriToString id (unProkkiBaseUrl prokkiBaseUrl) "")
      let ProjectCacheTtl ttl = projectCacheTtl
      log I $ "Project Cache TTL: " <> T.pack (show ttl)
      mapM_ (log I . renderIndex) (M.elems indexes)
      log I $ "Log severity: " <> T.pack (show logSeverity)
      log I $ "Response timeout: " <> T.pack (show responseTimeout)
      log I $ renderPGConfig pg
      log I $ renderS3Config s3

    cmanager <- C.newManager C.tlsManagerSettings
    requestCounters <- newTVarIO M.empty
    uploadRegistry <- newTVarIO M.empty
    projectCache <- newTVarIO PSQ.empty
    packageCache <- newTVarIO PSQ.empty
    startTime <- getCurrentTime
    pgPool <- mkPgPool pg
    s3Env <- mkS3Env (s3Host s3) (s3Port s3) (s3Bucket s3) (s3Secure s3)
    let mainLogAction = filterBySeverity logSeverity msgSeverity logAction
        prokkiEnv :: ProkkiEnv
        prokkiEnv =
          Env
            { envAddress = address,
              envProkkiBaseUrl = prokkiBaseUrl,
              envProjectCacheTtl = projectCacheTtl,
              envIndexes = indexes,
              envManager = cmanager,
              envLogAction = hoistLogAction liftIO mainLogAction,
              envRequestCounters = requestCounters,
              envUploadRegistry = uploadRegistry,
              envProjectCache = projectCache,
              envProjectCacheMaxSize = projectCacheMaxSize,
              envPackageCache = packageCache,
              envPackageCacheMaxSize = packageCacheMaxSize,
              envStartTime = startTime,
              envResponseTimeout = ResponseTimeout (responseTimeout * 1000000),
              envPgPool = pgPool,
              envS3 = s3Env
            }

    run (addressPort address) $ gzip defaultGzipSettings $ logRequests mainLogAction (prokkiApp prokkiEnv)

main :: IO ()
main = runApp =<< execParser opts
  where
    opts = info (argsParser <**> helper) (fullDesc <> progDesc "Prokki - Python packages index cache")

argsParser :: Parser Args
argsParser = Args <$> strOption (long "config" <> metavar "CONFIG" <> help "Configuration file path")

renderAddress :: Address -> T.Text
renderAddress (Address host port) = host <> ":" <> T.pack (show port)

renderIndex :: Index -> T.Text
renderIndex (Index name origin path) = "Index: " <> unIndexName name <> " -> " <> origin <> path

renderPGConfig :: PGConfig -> T.Text
renderPGConfig (PGConfig host port db) =
  "Postgres host: " <> host <> ", port: " <> T.pack (show port) <> ", db: " <> db

renderS3Config :: S3Config -> T.Text
renderS3Config (S3Config host port bucket secure) =
  "S3 host: " <> host <> ", port: " <> T.pack (show port) <> ", bucket: " <> bucket <> ", secure: " <> T.pack (show secure)
