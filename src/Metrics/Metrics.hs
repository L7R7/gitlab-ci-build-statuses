{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Metrics.Metrics
  ( OutgoingHttpRequestsHistogram (..),
    Metrics (..),
    registerMetrics,
    updateMetricsRegularly,
    metricsApiToIO,
    DurationObservation,
    observeDuration,
    observeDurationToIO,
  )
where

import Core.BuildStatuses (BuildStatus, BuildStatusesApi, Result (..), getStatuses, isHealthy)
import qualified Core.BuildStatuses as B (BuildStatuses (..))
import Core.Runners (RunnersJobsApi, getJobs)
import qualified Core.Runners as R (RunnersJobs (..))
import Core.Shared (Group, Id (..))
import Data.List (partition)
import Data.List.Extra (enumerate)
import Data.Map hiding (partition)
import Data.Text (toLower)
import GHC.Clock (getMonotonicTime)
import Metrics.PrometheusUtils (VectorWithLabel (VectorWithLabel))
import Polysemy
import qualified Polysemy.Reader as R
import Polysemy.Time (Seconds (Seconds), Time)
import qualified Polysemy.Time as Time
import Prometheus hiding (observeDuration)
import Prometheus.Metric.GHC
import Relude

registerMetrics :: IO Metrics
registerMetrics = registerGhcMetrics >> registerAppMetrics

registerGhcMetrics :: IO GHCMetrics
registerGhcMetrics = register ghcMetrics

registerAppMetrics :: IO Metrics
registerAppMetrics =
  Metrics <$> registerPipelinesOverviewMetric
    <*> registerOutgoingHttpRequestsHistogram
    <*> registerUpdateJobDurationHistogram
    <*> registerOnlineRunnersMetric
    <*> registerRunningJobsMetric

registerPipelinesOverviewMetric :: IO PipelinesOverviewGauge
registerPipelinesOverviewMetric = PipelinesOverviewGauge <$> register (vector ("group_id", "build_status") $ gauge (Info "build_pipelines_by_status_gauge" "Gauge that indicates the count of the pipeline statuses grouped by their result"))

registerOnlineRunnersMetric :: IO OnlineRunnersGauge
registerOnlineRunnersMetric = OnlineRunnersGauge <$> register (vector "group_id" $ gauge (Info "online_runners_gauge" "Gauge that indicates how many runners are online"))

registerRunningJobsMetric :: IO RunningJobsGauge
registerRunningJobsMetric = RunningJobsGauge <$> register (vector "group_id" $ gauge (Info "running_jobs_gauge" "Gauge that indicates how many jobs are running"))

registerOutgoingHttpRequestsHistogram :: IO OutgoingHttpRequestsHistogram
registerOutgoingHttpRequestsHistogram = OutgoingHttpRequestsHistogram <$> register (vector ("group_id", "path") $ histogram (Info "outgoing_http_requests_histogram" "Histogram indicating how long outgoing HTTP request durations") defaultBuckets)

registerUpdateJobDurationHistogram :: IO UpdateJobDurationHistogram
registerUpdateJobDurationHistogram = UpdateJobDurationHistogram <$> register (vector ("group_id", "tag") $ histogram (Info "update_job_duration_histogram" "Histogram indicating how long the update job took") (exponentialBuckets 0.5 1.25 20))

newtype PipelinesOverviewGauge = PipelinesOverviewGauge (Vector Label2 Gauge)

newtype OnlineRunnersGauge = OnlineRunnersGauge (Vector Label1 Gauge)

newtype RunningJobsGauge = RunningJobsGauge (Vector Label1 Gauge)

newtype OutgoingHttpRequestsHistogram = OutgoingHttpRequestsHistogram (Vector Label2 Histogram)

newtype UpdateJobDurationHistogram = UpdateJobDurationHistogram (Vector Label2 Histogram)

data Metrics = Metrics
  { currentPipelinesOverview :: !PipelinesOverviewGauge,
    outgoingHttpRequestsHistogram :: !OutgoingHttpRequestsHistogram,
    updateJobDurationHistogram :: !UpdateJobDurationHistogram,
    onlineRunnersGauge :: !OnlineRunnersGauge,
    runningJobsGauge :: !RunningJobsGauge
  }

data DurationObservation m a where
  ObserveDuration :: Text -> m b -> DurationObservation m b

makeSem ''DurationObservation

observeDurationToIO :: (Member (Embed IO) r, Member (R.Reader (Id Group)) r, Member (R.Reader UpdateJobDurationHistogram) r) => InterpreterFor DurationObservation r
observeDurationToIO sem = do
  groupId <- R.ask
  jobDurationHistogram <- R.ask
  observeDurationToIO' groupId jobDurationHistogram sem

observeDurationToIO' :: (Member (Embed IO) r) => Id Group -> UpdateJobDurationHistogram -> InterpreterFor DurationObservation r
observeDurationToIO' groupId@(Id gId) updateJobDurationHistogram@(UpdateJobDurationHistogram h) = interpretH $ \case
  ObserveDuration tag fb -> do
    start <- liftIO getMonotonicTime
    a <- runT fb
    res <- raise $ observeDurationToIO' groupId updateJobDurationHistogram a
    end <- liftIO getMonotonicTime
    let !timeTaken = end - start
    embed (observe (VectorWithLabel h (show gId, tag)) timeTaken :: IO ())
    pure res

data MetricsApi m a where
  UpdatePipelinesOverviewMetric :: B.BuildStatuses -> MetricsApi m ()
  UpdateHealthy :: Int -> MetricsApi m ()
  UpdateUnhealthy :: Int -> MetricsApi m ()
  UpdateOnlineRunners :: Int -> MetricsApi m ()
  UpdateRunningJobs :: Int -> MetricsApi m ()

makeSem ''MetricsApi

metricsApiToIO :: (Member (Embed IO) r, Member (R.Reader (Id Group)) r, Member (R.Reader PipelinesOverviewGauge) r, Member (R.Reader OnlineRunnersGauge) r, Member (R.Reader RunningJobsGauge) r) => InterpreterFor MetricsApi r
metricsApiToIO = interpret $ \case
  UpdatePipelinesOverviewMetric buildStatuses -> do
    groupId <- R.ask
    currentPipelinesOverview <- R.ask
    embed $ updatePipelinesOverviewMetricIO groupId currentPipelinesOverview buildStatuses
  UpdateHealthy healthyCount -> do
    groupId :: Id Group <- R.ask
    (PipelinesOverviewGauge currentPipelinesOverview) <- R.ask
    embed (withLabel currentPipelinesOverview (show groupId, "healthy") (`setGauge` fromIntegral healthyCount))
  UpdateUnhealthy unhealthyCount -> do
    groupId :: Id Group <- R.ask
    (PipelinesOverviewGauge currentPipelinesOverview) <- R.ask
    embed (withLabel currentPipelinesOverview (show groupId, "unhealthy") (`setGauge` fromIntegral unhealthyCount))
  UpdateOnlineRunners runners -> do
    groupId :: Id Group <- R.ask
    (OnlineRunnersGauge onlineRunnersGauge) <- R.ask
    embed (withLabel onlineRunnersGauge (show groupId) (`setGauge` fromIntegral runners))
  UpdateRunningJobs jobs -> do
    groupId :: Id Group <- R.ask
    (RunningJobsGauge runningJobsGauge) <- R.ask
    embed (withLabel runningJobsGauge (show groupId) (`setGauge` fromIntegral jobs))

updatePipelinesOverviewMetricIO :: Id Group -> PipelinesOverviewGauge -> B.BuildStatuses -> IO ()
updatePipelinesOverviewMetricIO _ _ B.NoSuccessfulUpdateYet = pass
updatePipelinesOverviewMetricIO groupId overviewGauge (B.Statuses (_, results)) = traverse_ (updateSingle groupId overviewGauge) (Data.Map.toList (countByBuildStatus results))

updateSingle :: Id Group -> PipelinesOverviewGauge -> (BuildStatus, Double) -> IO ()
updateSingle (Id groupId) (PipelinesOverviewGauge overviewGauge) (status, count) = withLabel overviewGauge (show groupId, (toLower . show) status) (`setGauge` count)

countByBuildStatus :: [Result] -> Map BuildStatus Double
countByBuildStatus results = countOccurrences buildStatus results `union` resetValues

resetValues :: Map BuildStatus Double
resetValues = Data.Map.fromList $ (,0) <$> enumerate

updateMetricsRegularly :: (Member BuildStatusesApi r, Member RunnersJobsApi r, Member MetricsApi r, Member (Time t d) r) => Sem r ()
updateMetricsRegularly = pass <$> infinitely (updateMetrics >> Time.sleep (Seconds 10))

updateMetrics :: (Member BuildStatusesApi r, Member RunnersJobsApi r, Member MetricsApi r) => Sem r ()
updateMetrics = updateBuildStatusesMetrics >> updateRunnersMetrics

updateBuildStatusesMetrics :: (Member BuildStatusesApi r, Member MetricsApi r) => Sem r ()
updateBuildStatusesMetrics = do
  statuses <- getStatuses
  updatePipelinesOverviewMetric statuses
  updateHealthyUnhealthy $ resultsFromBuildStatuses statuses

updateHealthyUnhealthy :: (Member MetricsApi r) => [Result] -> Sem r ()
updateHealthyUnhealthy results = do
  let (healthyCount, unhealthyCount) = bimap length length (partition (isHealthy . buildStatus) results)
  updateHealthy healthyCount
  updateUnhealthy unhealthyCount

resultsFromBuildStatuses :: B.BuildStatuses -> [Result]
resultsFromBuildStatuses (B.Statuses (_, res)) = res
resultsFromBuildStatuses B.NoSuccessfulUpdateYet = []

updateRunnersMetrics :: (Member RunnersJobsApi r, Member MetricsApi r) => Sem r ()
updateRunnersMetrics = do
  jobs <- getJobs
  updateRunningJobs $ runningJobs jobs
  updateOnlineRunners $ onlineRunners jobs

runningJobs :: R.RunnersJobs -> Int
runningJobs R.NoSuccessfulUpdateYet = 0
runningJobs (R.RunnersJobs (_, rs)) = getSum $ foldMap (Sum . length) rs

onlineRunners :: R.RunnersJobs -> Int
onlineRunners R.NoSuccessfulUpdateYet = 0
onlineRunners (R.RunnersJobs (_, rs)) = size rs

countOccurrences :: (Ord k, Num a) => (t -> k) -> [t] -> Map k a
countOccurrences f xs = fromListWith (+) [(f x, 1) | x <- xs]
