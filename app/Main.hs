module Main where

import           Config
import           Control.Concurrent.Async
import           Data.IORef
import           Data.Validation
import           Lib
import           Server
import           System.IO

main :: IO ()
main = do
  _ <- hSetBuffering stdout LineBuffering
  configFromEnv <- parseConfigFromEnv
  case configFromEnv of
    Success config -> do
      statuses <- newIORef []
      concurrently_ (updateStatusesRegularly config statuses) (runServer config statuses)
    Failure errors -> putStrLn $ showErrors errors
