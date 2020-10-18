{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import App (startWithConfig)
import Config (LogConfig (..), parseConfigFromEnv, showErrors)
import Data.Validation (Validation (Failure, Success))
import Katip
import Metrics.Metrics (registerMetrics)
import Outbound.Storage.InMemory (initStorage)
import RIO hiding (logError)
import RIO.Process (mkDefaultProcessContext)

main :: IO ()
main = do
  handleScribe <- mkHandleScribeWithFormatter jsonFormat (ColorLog False) stdout (permitItem InfoS) V2
  let mkLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "gitlab-ci-build-statuses" "production"
  bracket mkLogEnv closeScribes $ \logEnv -> do
    processContext <- mkDefaultProcessContext
    statuses <- initStorage
    metrics <- registerMetrics
    case parseConfigFromEnv metrics statuses (LogConfig mempty mempty logEnv) processContext of
      Success config -> do
        runKatipContextT logEnv () mempty $ logLocM InfoS . ls $ "Using config: " <> show config
        startWithConfig config
      Failure errs -> runKatipContextT logEnv () mempty $ logLocM ErrorS . ls $ "Failed to parse config. Exiting now. Errors were: " <> showErrors errs
