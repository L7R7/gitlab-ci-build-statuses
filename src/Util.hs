{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Util (parTraverseToIO) where

import Config.Config (MaxConcurrency (..))
import Core.Effects
import Polysemy
import Polysemy.Final (withWeavingToFinal)
import Polysemy.Reader qualified as R
import Relude
import UnliftIO.Internals.Async

parTraverseToIO :: (Member (Final IO) r, Member (R.Reader MaxConcurrency) r) => InterpreterFor ParTraverse r
parTraverseToIO =
  interpretH $ \case
    TraverseP f ta -> do
      maxConcurrency <- R.ask
      taT <- traverse pureT ta
      fT <- bindT f
      tb <- raise (parTraverseToIO (pooledMapConcurrentlySem maxConcurrency fT taT))
      ins <- getInspectorT
      pureT (catMaybes (inspect ins <$> catMaybes tb))

pooledMapConcurrentlySem :: Member (Final IO) r => MaxConcurrency -> (a -> Sem r b) -> [a] -> Sem r [Maybe b]
pooledMapConcurrentlySem (MaxConcurrency i) f t = withWeavingToFinal $ \s wv ins ->
  (<$ s) <$> pooledMapConcurrentlyIO i (\a -> ins <$> wv (f a <$ s)) t
