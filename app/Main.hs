{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import App (startWithConfig)
import Config (LogConfig (..), Validation (Failure, Success), parseConfigFromEnv, parseLogLevelWithDefault, showErrors)
import Control.Exception (bracket)
import Katip hiding (getEnvironment)
import Metrics.Metrics (registerMetrics)
import Metrics.Health(initThreads)
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
    case parseConfigFromEnv metrics statuses healthThreads (LogConfig mempty mempty logEnv) environment of
      Success config -> do
        runKatipContextT logEnv () mempty $ logLocM InfoS . ls $ "Using config: " <> (show @Text) config
        startWithConfig config
      Failure errs -> runKatipContextT logEnv () mempty $ logLocM ErrorS . ls $ "Failed to parse config. Exiting now. Errors were: " <> showErrors errs
