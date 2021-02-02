{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import App (startWithConfig)
import Config (LogConfig (..), Validation (Failure, Success), parseConfigFromEnv, parseLogLevelWithDefault, showErrors)
import Control.Exception (bracket)
import Katip
import Metrics.Metrics (registerMetrics)
import Outbound.Storage.InMemory (initStorage)
import RIO.Process (mkDefaultProcessContext)
import Relude

main :: IO ()
main = do
  processContext <- mkDefaultProcessContext
  let (logLevel, logLevelParseError) = parseLogLevelWithDefault processContext
  handleScribe <- mkHandleScribeWithFormatter jsonFormat (ColorLog False) stdout (permitItem logLevel) V2
  let mkLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "gitlab-ci-build-statuses" "production"
  bracket mkLogEnv closeScribes $ \logEnv -> do
    case logLevelParseError of
      Nothing -> pure ()
      Just err -> runKatipContextT logEnv () mempty $ logLocM WarningS . ls $ err
    statuses <- initStorage
    metrics <- registerMetrics
    case parseConfigFromEnv metrics statuses (LogConfig mempty mempty logEnv) processContext of
      Success config -> do
        runKatipContextT logEnv () mempty $ logLocM InfoS . ls $ "Using config: " <> (show @Text) config
        startWithConfig config
      Failure errs -> runKatipContextT logEnv () mempty $ logLocM ErrorS . ls $ "Failed to parse config. Exiting now. Errors were: " <> showErrors errs
