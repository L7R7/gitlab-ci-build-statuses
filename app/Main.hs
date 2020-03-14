module Main where

import           Config
import           Control.Concurrent.Async
import           Data.IORef
import           Data.Validation
import           Lib
import           Server
import           System.Environment

main :: IO ()
main = do
  env <- getEnvironment
  _ <- putStrLn $ unlines $ show <$> env
  configFromEnv <- parseConfigFromEnv
  case configFromEnv of
    Success config -> do
      statuses <- newIORef []
      concurrently_ (updateStatusesRegularly config statuses) (runServer config statuses)
    Failure errors -> putStrLn $ showErrors errors
