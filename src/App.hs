{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App (App (..)) where

import Config
import Core.Lib
import Env
import Inbound.HTTP.Metrics (HasOutgoingHttpRequestsHistogram (..), HasPipelinesOverviewGauge (..), Metrics (..), currentPipelinesOverview)
import Inbound.Jobs.Inbound.Jobs.Updating (HasDataUpdateInterval (..), HasUpdateJobDurationHistogram (..))
import Katip
import Prometheus (MonadMonitor (..))
import RIO

data App = App
  { statuses :: !(IORef BuildStatuses),
    config :: !Config,
    logNamespace :: !Namespace,
    logContext :: !LogContexts,
    logEnv :: !LogEnv,
    metrics :: !Metrics
  }

instance Katip (RIO App) where
  getLogEnv = asks logEnv
  localLogEnv f (RIO a) = RIO (local (\s -> s {logEnv = f (logEnv s)}) a)

instance KatipContext (RIO App) where
  getKatipContext = asks logContext
  localKatipContext f (RIO app) = RIO (local (\s -> s {logContext = f (logContext s)}) app)
  getKatipNamespace = asks logNamespace
  localKatipNamespace f (RIO app) = RIO (local (\s -> s {logNamespace = f (logNamespace s)}) app)

instance HasConfig App where
  configL = to config

instance HasApiToken App where
  apiTokenL = to (apiToken . config)

instance HasBaseUrl App where
  baseUrlL = to (gitlabBaseUrl . config)

instance HasGroupId App where
  groupIdL = to (groupId . config)

instance HasDataUpdateInterval App where
  dataUpdateIntervalL = to (dataUpdateIntervalSecs . config)

instance HasUiUpdateInterval App where
  uiUpdateIntervalL = to (uiUpdateIntervalSecs . config)

instance HasPipelinesOverviewGauge App where
  pipelinesOverviewGaugeL = to (currentPipelinesOverview . metrics)

instance HasOutgoingHttpRequestsHistogram App where
  outgoingHttpRequestsHistogramL = to (outgoingHttpRequestsHistogram . metrics)

instance HasUpdateJobDurationHistogram App where
  hasUpdateJobDurationHistogramL = to (updateJobDurationHistogram . metrics)

instance MonadMonitor (RIO App) where
  doIO = liftIO
