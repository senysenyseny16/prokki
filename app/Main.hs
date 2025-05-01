{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

import Colog (hoistLogAction, richMessageAction)
import Colog.Concurrent (defCapacity, withBackgroundLogger)
import Control.Monad.IO.Class (liftIO)
import qualified Network.HTTP.Conduit as C
import Network.Wai.Handler.Warp (run)
import Options.Applicative (execParser, fullDesc, helper, info, progDesc, (<**>))
import Parser
import Prokki.Env
import Prokki.Middleware.RequestLogger (logRequests)
import Prokki.Monad (ProkkiEnv)
import Prokki.Prokki (prokkiApp)
import Prokki.Type (Address (..))
import Prokki.Utils (noCompressionTlsManagerSettings)

runProkki :: Args -> IO ()
runProkki Args {..} = do
  -- runStdoutLoggingT $ do
  --  logInfoN ("Prokki v" <> prokkiVersion <> " on " <> T.pack (show address))
  --  logInfoN (T.pack (show index))
  --  logInfoN (T.pack (show cache))

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

    run (port address) $ logRequests logAction (prokkiApp prokkiEnv)

main :: IO ()
main = runProkki =<< execParser opts
  where
    opts = info (argsParser <**> helper) (fullDesc <> progDesc "Prokki - Python packages index cache")
