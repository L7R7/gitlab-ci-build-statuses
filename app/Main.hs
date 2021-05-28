{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import App (startWithConfig)
import Config.Backbone (Backbone (gitCommit), LogConfig (LogConfig), initBackbone)
import Config.Config (Validation (Failure, Success), logLevel, parseConfigFromEnv, showErrors)
import Katip hiding (getEnvironment)
import Logger (singleLog, withLogEnv)
import Metrics.Health (initThreads)
import Metrics.Metrics (registerMetrics)
import Outbound.Storage.InMemory (initStorage)
import Relude
import System.Environment

main :: IO ()
main = do
  environment <- getEnvironment
  statuses <- initStorage
  healthThreads <- initThreads
  metrics <- registerMetrics
  case parseConfigFromEnv environment of
    Success config -> withLogEnv (logLevel config) $ \logEnv -> do
      let backbone = initBackbone metrics statuses healthThreads (LogConfig mempty mempty logEnv)
      singleLog logEnv InfoS $ "Using config: " <> show config
      singleLog logEnv InfoS $ "Running version: " <> show (gitCommit backbone)
      startWithConfig config backbone
    Failure errs -> withLogEnv ErrorS $ \logEnv ->
      singleLog logEnv ErrorS $ "Failed to parse config. Exiting now. Errors were: " <> showErrors errs
