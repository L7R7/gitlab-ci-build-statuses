{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config
import           Control.Concurrent.Async
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Unlift
import           Data.IORef
import           Data.Validation
import           Lib
import           Server
import           System.IO

main :: IO ()
main =
  runStdoutLoggingT $ do
    _ <- liftIO $ hSetBuffering stdout LineBuffering
    configFromEnv <- parseConfigFromEnv
    case configFromEnv of
      Success config -> do
        statuses <- liftIO (newIORef [])
        concurrentlyT_ (updateStatusesRegularly config statuses) (runServer config statuses)
      Failure errors -> logErrorN "Failed to parse config. Shutting down"

concurrentlyT_ :: MonadBaseUnlift IO m => m a -> m b -> m () -- TODO: lriedisser 2020-03-18 Can monad-unlift do better than this?
concurrentlyT_ f g = do
  UnliftBase run <- askUnliftBase
  liftBase $ concurrently_ (run f) (run g)
