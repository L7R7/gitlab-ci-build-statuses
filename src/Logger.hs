{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Logger (loggerToIO, singleLog, withLogEnv) where

import Config.Backbone (LogConfig (..), logContext, logEnv, logNamespace)
import Control.Exception (bracket)
import Control.Lens (over, view)
import Core.Effects (Logger (..))
import Katip
import Polysemy
import Polysemy.Reader (Reader, asks, local)
import Relude hiding (Reader, asks, local)

loggerToIO :: (KatipContext (Sem r)) => InterpreterFor Logger r
loggerToIO = do
  interpretH $ \case
    LogDebug msg -> do
      raise $ logFM DebugS $ ls msg
      pureT ()
    LogInfo msg -> do
      raise $ logFM InfoS $ ls msg
      pureT ()
    LogWarn msg -> do
      raise $ logFM WarningS $ ls msg
      pureT ()
    LogError msg -> do
      raise $ logFM ErrorS $ ls msg
      pureT ()
    AddContext key payload action -> do
      a <- runT action
      raise $ katipAddContext (sl key payload) (loggerToIO a)
    AddContexts contexts action -> do
      a <- runT action
      -- TODO: 2020-10-18 does this make sense performance-wise?
      raise $ katipAddContext (foldl' (\acc (x, y) -> acc <> sl x y) mempty contexts) (loggerToIO a)
    AddNamespace namespace action -> do
      a <- runT action
      raise $ katipAddNamespace (Namespace [namespace]) (loggerToIO a)

instance (Members [Embed IO, Reader LogConfig] r) => Katip (Sem r) where
  getLogEnv = asks $ view logEnv
  localLogEnv f m = local (over logEnv f) m

instance (Members [Embed IO, Reader LogConfig] r) => KatipContext (Sem r) where
  getKatipContext = asks $ view logContext
  localKatipContext f m = local (over logContext f) m
  getKatipNamespace = asks $ view logNamespace
  localKatipNamespace f m = local (over logNamespace f) m

withLogEnv :: Severity -> (LogEnv -> IO b) -> IO b
withLogEnv logLevel action = do
  handleScribe <- mkHandleScribeWithFormatter jsonFormat (ColorLog False) stdout (permitItem logLevel) V2
  let mkLogEnv = initLogEnv "gitlab-ci-build-statuses" "production" >>= registerScribe "stdout" handleScribe defaultScribeSettings
  bracket mkLogEnv closeScribes action

singleLog :: LogEnv -> Severity -> Text -> IO ()
singleLog env severity logMessage = runKatipContextT env () mempty $ logLocM severity . ls $ logMessage
