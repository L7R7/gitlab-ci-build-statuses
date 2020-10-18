{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Core.Effects
  ( Delay (..),
    delaySeconds,
    Timer (..),
    getCurrentTime,
    ParTraverse (..),
    traverseP,
    Logger (..),
    addContext,
    addContexts,
    addNamespace,
    logInfo,
    logWarn,
    logError,
  )
where

import Data.Aeson (ToJSON)
import Data.Time (UTCTime)
import Polysemy
import RIO hiding (logError, logInfo, logWarn)

data Delay m a where
  DelaySeconds :: Int -> Delay m ()

makeSem ''Delay

data Timer m a where
  GetCurrentTime :: Timer m UTCTime

makeSem ''Timer

-- TODO: 2020-10-18: Generalize to Traversable&Monoid instead of List?
data ParTraverse m a where
  TraverseP :: (a -> m b) -> [a] -> ParTraverse m [b]

makeSem ''ParTraverse

data Logger m a where
  LogInfo :: Text -> Logger m ()
  LogWarn :: Text -> Logger m ()
  LogError :: Text -> Logger m ()
  AddContext :: ToJSON l => Text -> l -> m b -> Logger m b
  AddContexts :: ToJSON l => [(Text, l)] -> m b -> Logger m b
  AddNamespace :: Text -> m b -> Logger m b

makeSem ''Logger
