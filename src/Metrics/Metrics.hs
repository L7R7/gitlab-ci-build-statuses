{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Metrics.Metrics
  ( OnlineRunnersGauge (..),
    OutgoingHttpRequestsHistogram (..),
    PipelinesOverviewGauge (..),
    RunningJobsGauge (..),
    WaitingJobsGauge (..),
    UpdateJobDurationHistogram (..),
    CacheResultsCounter (..),
    Metrics (..),
    registerMetrics,
    updateMetricsRegularly,
    metricsApiToIO,
    DurationObservation,
    observeDuration,
    observeDurationToIO,
    recordCacheLookupResult,
    CacheTag (..),
    CacheResult (..),
    MetricsApi,
  )
where

import Core.BuildStatuses (BuildStatusesApi, Result (..), getStatuses, isHealthy)
import Core.BuildStatuses qualified as B (BuildStatuses (..))
import Core.Runners (RunnersJobsApi, getJobs)
import Core.Runners qualified as R (RunnersJobs (..))
import Data.List (partition)
import Data.List.Extra (enumerate)
import Data.Map hiding (partition)
import Data.Text (toLower)
import GHC.Clock (getMonotonicTime)
import Gitlab.Job (JobStatus)
import Metrics.PrometheusUtils (VectorWithLabel (VectorWithLabel))
import Polysemy
import Polysemy.Reader qualified as R
import Polysemy.Time (Seconds (Seconds), Time)
import Polysemy.Time qualified as Time
import Prometheus hiding (observeDuration)
import Prometheus.Metric.GHC
import Relude

registerMetrics :: IO Metrics
registerMetrics = registerGhcMetrics >> registerAppMetrics

registerGhcMetrics :: IO GHCMetrics
registerGhcMetrics = register ghcMetrics

registerAppMetrics :: IO Metrics
registerAppMetrics =
  Metrics
    <$> registerPipelinesOverviewMetric
    <*> registerOutgoingHttpRequestsHistogram
    <*> registerUpdateJobDurationHistogram
    <*> registerCacheResultsCounter
    <*> registerOnlineRunnersMetric
    <*> registerRunningJobsMetric
    <*> registerWaitingJobsMetric

registerPipelinesOverviewMetric :: IO PipelinesOverviewGauge
registerPipelinesOverviewMetric = PipelinesOverviewGauge <$> register (vector "build_status" $ gauge (Info "build_pipelines_by_status_gauge" "Gauge that indicates the count of the pipeline statuses grouped by their result"))

registerOnlineRunnersMetric :: IO OnlineRunnersGauge
registerOnlineRunnersMetric = OnlineRunnersGauge <$> register (gauge (Info "online_runners_gauge" "Gauge that indicates how many runners are online"))

registerRunningJobsMetric :: IO RunningJobsGauge
registerRunningJobsMetric = RunningJobsGauge <$> register (gauge (Info "running_jobs_gauge" "Gauge that indicates how many jobs are running"))

registerWaitingJobsMetric :: IO WaitingJobsGauge
registerWaitingJobsMetric = WaitingJobsGauge <$> register (gauge (Info "waiting_jobs_gauge" "Gauge that indicates how many jobs are waiting"))

registerOutgoingHttpRequestsHistogram :: IO OutgoingHttpRequestsHistogram
registerOutgoingHttpRequestsHistogram = OutgoingHttpRequestsHistogram <$> register (vector "path" $ histogram (Info "outgoing_http_requests_histogram" "Histogram indicating how long outgoing HTTP request durations") defaultBuckets)

registerUpdateJobDurationHistogram :: IO UpdateJobDurationHistogram
registerUpdateJobDurationHistogram = UpdateJobDurationHistogram <$> register (vector "tag" $ histogram (Info "update_job_duration_histogram" "Histogram indicating how long the update job took") (exponentialBuckets 0.5 1.25 20))

registerCacheResultsCounter :: IO CacheResultsCounter
registerCacheResultsCounter = CacheResultsCounter <$> register (vector ("tag", "result") $ counter (Info "cache_results_count" "Counter that indicates the results of the different caches. Includes labels for group id, cache tag, and result(hit/miss)"))

newtype PipelinesOverviewGauge = PipelinesOverviewGauge (Vector Label1 Gauge)

newtype OnlineRunnersGauge = OnlineRunnersGauge Gauge

newtype RunningJobsGauge = RunningJobsGauge Gauge

newtype WaitingJobsGauge = WaitingJobsGauge Gauge

newtype OutgoingHttpRequestsHistogram = OutgoingHttpRequestsHistogram (Vector Label1 Histogram)

newtype UpdateJobDurationHistogram = UpdateJobDurationHistogram (Vector Label1 Histogram)

newtype CacheResultsCounter = CacheResultsCounter (Vector Label2 Counter)

data Metrics = Metrics
  { currentPipelinesOverview :: !PipelinesOverviewGauge,
    outgoingHttpRequestsHistogram :: !OutgoingHttpRequestsHistogram,
    updateJobDurationHistogram :: !UpdateJobDurationHistogram,
    cacheResultsCounter :: CacheResultsCounter,
    onlineRunnersGauge :: !OnlineRunnersGauge,
    runningJobsGauge :: !RunningJobsGauge,
    waitingJobsGauge :: !WaitingJobsGauge
  }

data DurationObservation m a where
  ObserveDuration :: Text -> m b -> DurationObservation m b

makeSem ''DurationObservation

observeDurationToIO :: (Member (Embed IO) r, Member (R.Reader UpdateJobDurationHistogram) r) => InterpreterFor DurationObservation r
observeDurationToIO sem = do
  jobDurationHistogram <- R.ask
  observeDurationToIO' jobDurationHistogram sem

observeDurationToIO' :: (Member (Embed IO) r) => UpdateJobDurationHistogram -> InterpreterFor DurationObservation r
observeDurationToIO' updateJobDurationHistogram@(UpdateJobDurationHistogram h) = interpretH $ \case
  ObserveDuration tag fb -> do
    start <- liftIO getMonotonicTime
    a <- runT fb
    res <- raise $ observeDurationToIO' updateJobDurationHistogram a
    end <- liftIO getMonotonicTime
    let !timeTaken = end - start
    embed (observe (VectorWithLabel h tag) timeTaken :: IO ())
    pure res

newtype CacheTag = CacheTag Text

data CacheResult = Hit | Miss

cacheResultToProm :: CacheResult -> Text
cacheResultToProm Hit = "hit"
cacheResultToProm Miss = "miss"

data MetricsApi m a where
  UpdatePipelinesOverviewMetric :: B.BuildStatuses -> MetricsApi m ()
  UpdateHealthy :: Int -> MetricsApi m ()
  UpdateUnhealthy :: Int -> MetricsApi m ()
  UpdateOnlineRunners :: Int -> MetricsApi m ()
  UpdateRunningJobs :: Int -> MetricsApi m ()
  RecordCacheLookupResult :: CacheTag -> CacheResult -> MetricsApi m ()

makeSem ''MetricsApi

metricsApiToIO :: (Member (Embed IO) r, Member (R.Reader PipelinesOverviewGauge) r, Member (R.Reader OnlineRunnersGauge) r, Member (R.Reader RunningJobsGauge) r, Member (R.Reader CacheResultsCounter) r) => InterpreterFor MetricsApi r
metricsApiToIO sem = do
  pipelinesOverviewGauge <- R.ask
  onlineRunnersGauge <- R.ask
  runningJobsGauge <- R.ask
  cacheResultsCounter <- R.ask
  metricsApiToIO' pipelinesOverviewGauge onlineRunnersGauge runningJobsGauge cacheResultsCounter sem

metricsApiToIO' :: (Member (Embed IO) r) => PipelinesOverviewGauge -> OnlineRunnersGauge -> RunningJobsGauge -> CacheResultsCounter -> InterpreterFor MetricsApi r
metricsApiToIO' pipelinesOverviewGauge@(PipelinesOverviewGauge currentPipelinesOverview) (OnlineRunnersGauge onlineRunnersGauge) (RunningJobsGauge runningJobsGauge) (CacheResultsCounter cacheResults) = interpret $ \case
  UpdatePipelinesOverviewMetric buildStatuses -> embed $ updatePipelinesOverviewMetricIO pipelinesOverviewGauge buildStatuses
  UpdateHealthy healthyCount -> embed (withLabel currentPipelinesOverview "healthy" (`setGauge` fromIntegral healthyCount))
  UpdateUnhealthy unhealthyCount -> embed (withLabel currentPipelinesOverview "unhealthy" (`setGauge` fromIntegral unhealthyCount))
  UpdateOnlineRunners runners -> embed (setGauge onlineRunnersGauge (fromIntegral runners))
  UpdateRunningJobs jobs -> embed (setGauge runningJobsGauge (fromIntegral jobs))
  RecordCacheLookupResult (CacheTag tag) result -> embed (withLabel cacheResults (tag, cacheResultToProm result) incCounter)

updatePipelinesOverviewMetricIO :: PipelinesOverviewGauge -> B.BuildStatuses -> IO ()
updatePipelinesOverviewMetricIO _ B.NoSuccessfulUpdateYet = pass
updatePipelinesOverviewMetricIO overviewGauge (B.Statuses (_, results)) = traverse_ (updateSingle overviewGauge) (Data.Map.toList (countByBuildStatus results))

updateSingle :: PipelinesOverviewGauge -> (JobStatus, Double) -> IO ()
updateSingle (PipelinesOverviewGauge overviewGauge) (status, count) = withLabel overviewGauge ((toLower . show) status) (`setGauge` count)

countByBuildStatus :: [Result] -> Map JobStatus Double
countByBuildStatus results = countOccurrences buildStatus results `union` resetValues

resetValues :: Map JobStatus Double
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
