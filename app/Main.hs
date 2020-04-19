{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Colog
  ( logError,
    richMessageAction,
    usingLoggerT,
  )
import Config
import Data.Validation
import Lib
import RIO hiding (logError)
import Server

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
      Failure _ -> logError "Failed to parse config. Exiting now"
