{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config
import           Control.Concurrent.Async
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Log
import           Control.Monad.Log.Label
import           Control.Monad.Trans.Unlift
import           Data.IORef
import           Data.Validation
import           Lib
import           Prelude                    hiding (error)
import           Server
import           System.IO
import           System.Log.FastLogger

main :: IO ()
main = do
  logger <- makeDefaultJSONLogger simpleTimeFormat' (LogStdout 4096) levelDebug (Label "main") -- runLogTSafe logger $
  foo logger
  pure ()
  where
    foo logger = do
      _ <- hSetBuffering stdout LineBuffering
      configFromEnv <- runLogTSafe logger parseConfigFromEnv
      case configFromEnv of
        Success config -> do
          statuses <- liftIO (newIORef [])
          concurrently_ (runLogTSafe logger $ updateStatusesRegularly config statuses) (runLogTSafe logger $ runServer config statuses)
        Failure errors -> putStrLn "Failed to parse config. Shutting down" -- TODO: lriedisser 2020-03-19 fix this

concurrentlyT_ :: MonadBaseUnlift IO m => m a -> m b -> m () -- TODO: lriedisser 2020-03-18 Can monad-unlift do better than this?
concurrentlyT_ f g = do
  UnliftBase run <- askUnliftBase
  liftBase $ concurrently_ (run f) (run g)
