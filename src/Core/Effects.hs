{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Core.Effects
  ( ParTraverse (..),
    traverseP,
    Logger (..),
    addContext,
    addContexts,
    addNamespace,
    logDebug,
    logInfo,
    logWarn,
    logError,
    Health (..),
    isHealthy,
  )
where

import Data.Aeson (ToJSON)
import Polysemy
import Relude

-- TODO: 2020-10-18: Generalize to Traversable&Monoid instead of List?
data ParTraverse m a where
  TraverseP :: (a -> m b) -> [a] -> ParTraverse m [b]

makeSem ''ParTraverse

data Logger m a where
  LogDebug :: Text -> Logger m ()
  LogInfo :: Text -> Logger m ()
  LogWarn :: Text -> Logger m ()
  LogError :: Text -> Logger m ()
  AddContext :: ToJSON l => Text -> l -> m b -> Logger m b
  AddContexts :: ToJSON l => [(Text, l)] -> m b -> Logger m b
  AddNamespace :: Text -> m b -> Logger m b

makeSem ''Logger

data Health m a where
  IsHealthy :: Health m Bool

makeSem ''Health
