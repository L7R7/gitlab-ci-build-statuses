{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Config
import Data.Validation
import Env
import Katip
import Lib
import RIO hiding (logError)
import RIO.Process (mkDefaultProcessContext)
import Server

main :: IO ()
main = do
  handleScribe <- mkHandleScribeWithFormatter jsonFormat ColorIfTerminal stdout (permitItem InfoS) V2
  let mkLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "gitlab-ci-build-statuses" "production"
  bracket mkLogEnv closeScribes $ \le -> do
    pc <- mkDefaultProcessContext
    let configFromEnv = parseConfigFromEnv pc
    case configFromEnv of
      Success config -> do
        statuses <- liftIO (newIORef [])
        let app = App statuses config mempty mempty le
        runRIO app (concurrently_ updateStatusesRegularly startServer)
      Failure errs -> runKatipContextT le () mempty $ logLocM ErrorS . ls $ "Failed to parse config. Exiting now. Errors were: " <> showErrors errs
