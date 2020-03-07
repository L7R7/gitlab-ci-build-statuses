module Main where

import           Config
import           Control.Concurrent.Async
import           Data.IORef
import           Data.Validation
import           Lib
import           Server

main :: IO ()
main = do
  configFromEnv <- parseConfigFromEnv
  case configFromEnv of
    Success config -> do
      statuses <- newIORef []
      concurrently_ (updateStatusesRegularly config statuses) (runServer statuses)
    Failure errors -> putStrLn $ showErrors errors
