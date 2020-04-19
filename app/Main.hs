{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Colog
  ( logError,
    logInfo,
    richMessageAction,
    usingLoggerT,
  )
import Config
import Control.Concurrent.Async
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Unlift
import Data.IORef
import Data.Validation
import Lib
import Server
import System.IO

main :: IO ()
main =
  usingLoggerT richMessageAction $ do
    _ <- liftIO $ hSetBuffering stdout LineBuffering
    configFromEnv <- parseConfigFromEnv
    case configFromEnv of
      Success config -> do
        statuses <- liftIO (newIORef [])
        liftIO $
          concurrently_ -- TODO: lriedisser 2020-03-21 Can something like monad-unlift do better than this?
            (usingLoggerT richMessageAction $ updateStatusesRegularly config statuses)
            (usingLoggerT richMessageAction $ runServer config statuses)
      Failure errors -> logError "Failed to parse config. Shutting down"
