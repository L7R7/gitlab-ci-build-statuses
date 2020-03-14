module Main where

import           Config
import           Control.Concurrent.Async
import           Data.IORef
import           Lib
import           Server
import           System.Environment

main :: IO ()
main = do
  env <- getEnvironment
  _ <- putStrLn $ unlines $ show <$> env
  config <- parseConfig
  statuses <- newIORef []
  concurrently_ (updateStatusesRegularly config statuses) (runServer config statuses)
