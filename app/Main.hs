{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import App (startWithConfig)
import Config (LogConfig (..), Validation (Failure, Success), parseConfigFromEnv, showErrors)
import Control.Exception (bracket)
import Katip
import Metrics.Metrics (registerMetrics)
import Outbound.Storage.InMemory (initStorage)
import RIO.Process (mkDefaultProcessContext)
import Relude

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
        runKatipContextT logEnv () mempty $ logLocM InfoS . ls $ "Using config: " <> (show @Text) config
        startWithConfig config
      Failure errs -> runKatipContextT logEnv () mempty $ logLocM ErrorS . ls $ "Failed to parse config. Exiting now. Errors were: " <> showErrors errs
