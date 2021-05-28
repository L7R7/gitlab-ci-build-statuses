{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Util (parTraverseToIO) where

import Config.Config (MaxConcurrency (..))
import Core.Effects
import Polysemy
import Polysemy.Final (withWeavingToFinal)
import Relude
import UnliftIO.Internals.Async

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
