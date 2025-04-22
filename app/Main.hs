{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad.Logger (logInfoN, runStdoutLoggingT)
import qualified Data.Text as T
import qualified Network.HTTP.Conduit as C
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Options.Applicative (execParser, fullDesc, helper, info, progDesc, (<**>))
import Parser
import qualified Prokki.Env as E
import Prokki.Prokki (prokkiApp)
import Prokki.Utils (noCompressionTlsManagerSettings, prokkiVersion)

runProkki :: Args -> IO ()
runProkki Args {..} = do
  runStdoutLoggingT $ do
    logInfoN ("Prokki v" <> prokkiVersion <> " on " <> T.pack (show address))
    logInfoN (T.pack (show index))
    logInfoN (T.pack (show cache))

  cmanager <- C.newManager noCompressionTlsManagerSettings
  let env = E.Env {E.address = address, E.index = index, E.cache = cache, E.manager = cmanager}
  run (E.port address) $ logStdout (prokkiApp env)

main :: IO ()
main = runProkki =<< execParser opts
  where
    opts = info (argsParser <**> helper) (fullDesc <> progDesc "Prokki - Python packages index cache")
