{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Network.HTTP.Conduit as C
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Options.Applicative (execParser, fullDesc, helper, info, progDesc, (<**>))
import Parser (settingsParser)
import Prokki.Prokki (prokki)
import Prokki.Settings
import Prokki.Utils (prokkiVersion)

runProkki :: Settings -> IO ()
runProkki settings@Settings {..} = do
  print $ "Prokki v" ++ prokkiVersion ++ " on " ++ show address
  print $ show index
  print $ show cache

  manager <- C.newManager C.tlsManagerSettings
  run (port address) $ logStdout (prokki settings manager)

main :: IO ()
main = runProkki =<< execParser opts
  where
    opts = info (settingsParser <**> helper) (fullDesc <> progDesc "Prokki - Python packages index cache")
