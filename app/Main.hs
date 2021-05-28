{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import App (startWithConfig)
import Config.Backbone (LogConfig (LogConfig), parseBackboneFromEnv, parseLogLevelWithDefault)
import Config.Config (Validation (Failure, Success), parseConfigFromEnv, showErrors)
import Control.Exception (bracket)
import Katip hiding (getEnvironment)
import Metrics.Health (initThreads)
import Metrics.Metrics (registerMetrics)
import Outbound.Storage.InMemory (initStorage)
import Relude
import System.Environment

main :: IO ()
main = do
  environment <- getEnvironment
  let (logLevel, logLevelParseError) = parseLogLevelWithDefault environment
  handleScribe <- mkHandleScribeWithFormatter jsonFormat (ColorLog False) stdout (permitItem logLevel) V2
  let mkLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "gitlab-ci-build-statuses" "production"
  bracket mkLogEnv closeScribes $ \logEnv -> do
    traverse_ (runKatipContextT logEnv () mempty . logLocM WarningS . ls) logLevelParseError
    statuses <- initStorage
    healthThreads <- initThreads
    metrics <- registerMetrics
    let backbone = parseBackboneFromEnv metrics statuses healthThreads (LogConfig mempty mempty logEnv)
    case parseConfigFromEnv environment of
      Success config -> do
        runKatipContextT logEnv () mempty $ logLocM InfoS . ls $ "Using config: " <> (show @Text) config
        startWithConfig config backbone
      Failure errs -> runKatipContextT logEnv () mempty $ logLocM ErrorS . ls $ "Failed to parse config. Exiting now. Errors were: " <> showErrors errs
