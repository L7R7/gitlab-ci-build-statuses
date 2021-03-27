{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Util (delayToIO, timerToIO, parTraverseToIO) where

import Config (MaxConcurrency (..))
import Control.Concurrent (threadDelay)
import Core.Effects
import qualified Data.Time as T (getCurrentTime)
import Polysemy
import Polysemy.Final (withWeavingToFinal)
import Relude
import UnliftIO.Internals.Async

delayToIO :: (Member (Embed IO) r) => InterpreterFor Delay r
delayToIO = interpret $ \case
  DelaySeconds i -> embed (threadDelay (i * oneSecond) :: IO ())

oneSecond :: Int
oneSecond = 1000000

timerToIO :: (Member (Embed IO) r) => InterpreterFor Timer r
timerToIO = interpret $ \case
  GetCurrentTime -> embed T.getCurrentTime

parTraverseToIO :: (Member (Final IO) r) => MaxConcurrency -> InterpreterFor ParTraverse r
parTraverseToIO maxConcurrency =
  interpretH $ \case
    TraverseP f ta -> do
      taT <- traverse pureT ta
      fT <- bindT f
      tb <- raise (parTraverseToIO maxConcurrency (pooledMapConcurrentlySem maxConcurrency fT taT))
      ins <- getInspectorT
      pureT (catMaybes (inspect ins <$> catMaybes tb))

pooledMapConcurrentlySem :: Member (Final IO) r => MaxConcurrency -> (a -> Sem r b) -> [a] -> Sem r [Maybe b]
pooledMapConcurrentlySem (MaxConcurrency i) f t = withWeavingToFinal $ \s wv ins ->
  (<$ s) <$> pooledMapConcurrentlyIO i (\a -> ins <$> wv (f a <$ s)) t
