module Main where

import           Config
import           Control.Concurrent.Async
import           Data.IORef
import           Lib
import           Server

main :: IO ()
main = do
  config <- parseConfig
  statuses <- newIORef []
  concurrently_ (updateStatusesRegularly config statuses) (runServer config statuses)
