{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Config (Config, parseConfigFromEnv, showErrors)
import Control.Concurrent (forkIO)
import Data.Validation (Validation (Failure, Success))
import App (App (App))
import Katip
import Lib (updateStatusesRegularly)
import Metrics
import RIO hiding (logError)
import RIO.Process (mkDefaultProcessContext)
import Server (startServer)
import System.Metrics
import Data.Time

main :: IO ()
main = do
  handleScribe <- mkHandleScribeWithFormatter jsonFormat (ColorLog False) stdout (permitItem InfoS) V2
  let mkLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "gitlab-ci-build-statuses" "production"
  bracket mkLogEnv closeScribes $ \le -> do
    pc <- mkDefaultProcessContext
    case parseConfigFromEnv pc of
      Success config -> start config le
      Failure errs -> runKatipContextT le () mempty $ logLocM ErrorS . ls $ "Failed to parse config. Exiting now. Errors were: " <> showErrors errs

start :: Config -> LogEnv -> IO ()
start config le = do
  minTime <- getCurrentTime
  statuses <- newIORef (minTime, [])
  store <- newStore
  let app = App statuses config store mempty mempty le
  _ <- forkIO $ runRIO app startServer
  _ <- forkIO $ runRIO app updateStatusesRegularly
  initPrometheusMetrics store
  pure ()
