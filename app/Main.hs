{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad.Logger (logInfoN, runStdoutLoggingT)
import qualified Data.Text as T
import qualified Network.HTTP.Conduit as C
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Options.Applicative (execParser, fullDesc, helper, info, progDesc, (<**>))
import Parser
import Prokki.Env
import Prokki.Monad (ProkkiEnv)
import Prokki.Prokki (prokkiApp)
import Prokki.Type (Address (..))
import Prokki.Utils (noCompressionTlsManagerSettings, prokkiVersion)
import Prokki.Middleware.RequestLogger (logRequests)
import Colog (richMessageAction, cmapM, LogAction, Message)

runProkki :: Args -> IO ()
runProkki Args {..} = do
  --runStdoutLoggingT $ do
  --  logInfoN ("Prokki v" <> prokkiVersion <> " on " <> T.pack (show address))
  --  logInfoN (T.pack (show index))
  --  logInfoN (T.pack (show cache))

  let logAction = richMessageAction
  cmanager <- C.newManager noCompressionTlsManagerSettings
  let prokkiEnv :: ProkkiEnv
      prokkiEnv = Env {envAddress = address, envIndex = index, envCache = cache, envManager = cmanager, envLogAction = logAction}

  run (port address) $ logRequests logAction (prokkiApp prokkiEnv)

main :: IO ()
main = runProkki =<< execParser opts
  where
    opts = info (argsParser <**> helper) (fullDesc <> progDesc "Prokki - Python packages index cache")


