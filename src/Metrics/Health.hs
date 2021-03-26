{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Metrics.Health (getCurrentHealthStatus, initThreads, healthToIO) where

import Control.Concurrent (ThreadId)
import Control.Exception (throw)
import Core.Effects (Health (IsHealthy), isHealthy)
import GHC.Conc (ThreadStatus (ThreadDied, ThreadFinished), threadStatus)
import Polysemy
import Relude
import Servant (err503)

getCurrentHealthStatus :: Member Health r => Sem r Text
getCurrentHealthStatus = ifM isHealthy (pure "UP") (throw err503)

healthToIO :: Member (Embed IO) r => IORef [(ThreadId, Text)] -> InterpreterFor Health r
healthToIO ioref = interpret $ \case
  IsHealthy -> embed $ do
    threads <- readIORef ioref
    allM (isThreadHealthy . fst) threads

isThreadHealthy :: ThreadId -> IO Bool
isThreadHealthy = fmap (`notElem` [ThreadFinished, ThreadDied]) . threadStatus

initThreads :: IO (IORef [(ThreadId, Text)])
initThreads = newIORef []
