{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import App (App (App))
import Config (Config, parseConfigFromEnv, showErrors)
import Control.Concurrent (forkIO)
import Core.Lib (BuildStatuses (NoSuccessfulUpdateYet), updateStatusesRegularly)
import Data.Validation (Validation (Failure, Success))
import Inbound.HTTP.Server (startServer)
import Katip
import Outbound.Gitlab.GitlabAPI ()
import Outbound.Storage.InMemory ()
import RIO hiding (logError)
import RIO.Process (mkDefaultProcessContext)

main :: IO ()
main = do
  handleScribe <- mkHandleScribeWithFormatter jsonFormat (ColorLog False) stdout (permitItem InfoS) V2
  let mkLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "gitlab-ci-build-statuses" "production"
  bracket mkLogEnv closeScribes $ \logEnv -> do
    processContext <- mkDefaultProcessContext
    case parseConfigFromEnv processContext of
      Success config -> startWithEnv config logEnv
      Failure errs -> runKatipContextT logEnv () mempty $ logLocM ErrorS . ls $ "Failed to parse config. Exiting now. Errors were: " <> showErrors errs

startWithEnv :: Config -> LogEnv -> IO ()
startWithEnv config le = do
  statuses <- newIORef NoSuccessfulUpdateYet
  let app = App statuses config mempty mempty le
  _ <- forkIO $ runRIO app updateStatusesRegularly
  runRIO app startServer
