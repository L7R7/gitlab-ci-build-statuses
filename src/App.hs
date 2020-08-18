{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App where

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
  configL = lens config (\app iJC -> app {config = iJC})

instance HasApiToken App where
  apiTokenL = lens (apiToken . config) (\app token -> app {config = (config app) {apiToken = token}})

instance HasBaseUrl App where
  baseUrlL = lens (gitlabBaseUrl . config) (\app u -> app {config = (config app) {gitlabBaseUrl = u}})

instance HasGroupId App where
  groupIdL = lens (groupId . config) (\app g -> app {config = (config app) {groupId = g}})

instance HasDataUpdateInterval App where
  dataUpdateIntervalL = lens (dataUpdateIntervalMins . config) (\app d -> app {config = (config app) {dataUpdateIntervalMins = d}})

instance HasUiUpdateInterval App where
  uiUpdateIntervalL = lens (uiUpdateIntervalSecs . config) (\app u -> app {config = (config app) {uiUpdateIntervalSecs = u}})

instance HasPipelinesOverviewGauge App where
  pipelinesOverviewGaugeL = lens (currentPipelinesOverview . metrics) (\app cpo -> app {metrics = (metrics app) {currentPipelinesOverview = cpo}})

instance HasOutgoingHttpRequestsHistogram App where
  outgoingHttpRequestsHistogramL = lens (outgoingHttpRequestsHistogram . metrics) (\app ohrh -> app {metrics = (metrics app) {outgoingHttpRequestsHistogram = ohrh}})

instance HasUpdateJobDurationHistogram App where
  hasUpdateJobDurationHistogramL = lens (updateJobDurationHistogram . metrics) (\app ujdh -> app {metrics = (metrics app) {updateJobDurationHistogram = ujdh}})

instance MonadMonitor (RIO App) where
  doIO = liftIO
