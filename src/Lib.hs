{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( toMetricValue
  , updateStatuses
  , updateStatusesRegularly
  , BuildStatus(..)
  , Result(..)
  , GroupId(..)
  ) where

import           Colog
import           Config                  hiding (apiToken, groupId)
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              hiding (Result)
import           Data.Either.Combinators
import           Data.IORef
import           Data.List
import qualified Data.Text               as T hiding (partition)
import           Data.Time
import           Network.HTTP.Simple
import           Prelude                 hiding (id)
import           TextShow

data UpdateError
  = HttpError HttpException
  | EmptyPipelinesResult
  deriving (Show)

updateStatusesRegularly :: Config -> IORef [Result] -> LoggerT Message IO ()
updateStatusesRegularly config ioref =
  forever $ do
    logInfo "updating build statuses"
    results <- updateStatuses config ioref
    currentTime <- liftIO getCurrentTime
    logInfo $ T.unwords ["Done updating.", showt $ length results, "results. Update finished at", (T.pack . show) currentTime]
    liftIO . threadDelay $ calculateDelay (dataUpdateIntervalMins config)

calculateDelay :: DataUpdateIntervalMinutes -> Int
calculateDelay (DataUpdateIntervalMinutes mins) = mins * 60 * oneSecond

oneSecond :: Int
oneSecond = 1000000

updateStatuses :: Config -> IORef [Result] -> LoggerT Message IO [Result]
updateStatuses config ioref = do
  results <- currentKnownBuildStatuses config
  liftIO $ atomicModifyIORef' ioref (const (results, results))

currentKnownBuildStatuses :: Config -> LoggerT Message IO [Result]
currentKnownBuildStatuses config = do
  statuses <- currentBuildStatuses config
  pure $ filter (\r -> buildStatus r /= Unknown) statuses

currentBuildStatuses :: Config -> LoggerT Message IO [Result]
currentBuildStatuses (Config apiToken groupId baseUrl _ _) = do
  projects <- findProjects apiToken baseUrl groupId
  statuses <- traverse (evalProject apiToken baseUrl) projects
  pure $ sortOn (T.toLower . name) statuses

evalProject :: ApiToken -> BaseUrl -> Project -> LoggerT Message IO Result
evalProject apiToken baseUrl (Project id name pUrl) = do
  logInfo $ T.unwords ["Getting build status for project", showt id, "-", name]
  maybeBuildStatus <- findBuildStatus apiToken baseUrl (ProjectId id)
  status <-
    case maybeBuildStatus of
      Left EmptyPipelinesResult -> do
        logInfo $ T.unwords ["No pipelines found for project", showt id]
        pure Unknown
      Left uError -> do
        logInfo $ T.unwords ["Couldn't eval project with id", showt id, "- error was", (T.pack . show) uError]
        pure Unknown
      Right st -> pure $ toBuildStatus st
  pure $ Result name status pUrl

findBuildStatus :: ApiToken -> BaseUrl -> ProjectId -> LoggerT Message IO (Either UpdateError T.Text)
findBuildStatus apiToken baseUrl id = do
  pipelines <- liftIO $ fetchData apiToken $ pipelinesRequest baseUrl id
  pure $ pipelineStatus <$> (pipelines >>= maxByPipelineId)

findProjects :: ApiToken -> BaseUrl -> GroupId -> LoggerT Message IO [Project]
findProjects apiToken baseUrl groupId = do
  result <- liftIO $ fetchData apiToken $ projectsRequest baseUrl groupId
  case result of
    Left err -> do
      logInfo $ T.unwords ["Couldn't load projects. Error was", (T.pack . show) err]
      pure []
    Right ps -> pure ps

maxByPipelineId :: [Pipeline] -> Either UpdateError Pipeline
maxByPipelineId []        = Left EmptyPipelinesResult
maxByPipelineId pipelines = Right $ maximum pipelines

fetchData :: FromJSON a => ApiToken -> Request -> IO (Either UpdateError [a])
fetchData (ApiToken apiToken) request = do
  result <- try (getResponseBody <$> httpJSON (setRequestHeader "PRIVATE-TOKEN" [apiToken] request))
  pure $ mapLeft HttpError result

projectsRequest :: BaseUrl -> GroupId -> Request
projectsRequest (BaseUrl baseUrl) (GroupId groupId) =
  parseRequest_ $ mconcat [baseUrl, "/api/v4/groups/", show groupId, "/projects?per_page=100&simple=true&include_subgroups=true"]

-- TODO: lriedisser 2020-03-06 order by updated_at?
pipelinesRequest :: BaseUrl -> ProjectId -> Request
pipelinesRequest (BaseUrl baseUrl) (ProjectId i) = parseRequest_ $ mconcat [baseUrl, "/api/v4/projects/", show i, "/pipelines?ref=master"]

data Project =
  Project
    { projectId   :: Int
    , projectName :: T.Text
    , projectUrl  :: T.Text
    }
  deriving (Show)

instance FromJSON Project where
  parseJSON = withObject "Project" $ \p -> Project <$> p .: "id" <*> p .: "name" <*> p .: "web_url"

data Pipeline =
  Pipeline
    { pipelineId     :: Int
    , ref            :: T.Text
    , pipelineStatus :: T.Text
    , pipelineUrl    :: T.Text
    }
  deriving (Show)

instance Eq Pipeline where
  (==) p1 p2 = pipelineId p1 == pipelineId p2

instance Ord Pipeline where
  (<=) p1 p2 = pipelineId p1 <= pipelineId p2

instance FromJSON Pipeline where
  parseJSON = withObject "Pipeline" $ \p -> Pipeline <$> p .: "id" <*> p .: "ref" <*> p .: "status" <*> p .: "web_url"

data BuildStatus
  = Unknown
  | Running
  | Failed
  | Cancelled
  | Pending
  | Skipped
  | Successful
  deriving (Eq, Show, Ord)

instance TextShow BuildStatus where
  showb = showb . show

toBuildStatus :: T.Text -> BuildStatus
toBuildStatus "success"  = Successful
toBuildStatus "running"  = Running
toBuildStatus "failed"   = Failed
toBuildStatus "canceled" = Cancelled
toBuildStatus "pending"  = Pending
toBuildStatus "skipped"  = Skipped
toBuildStatus _          = Unknown

-- | map a build status to a float value that's suitable for displaying it on Grafana dashboard.
-- Suggested colors are:
-- 0 -> green
-- 1 -> red
-- >1 -> blue
--
toMetricValue :: BuildStatus -> Float
toMetricValue Successful = 0
toMetricValue Failed     = 1
toMetricValue Unknown    = 2
toMetricValue Running    = 3
toMetricValue Cancelled  = 4
toMetricValue Pending    = 5
toMetricValue Skipped    = 6

data Result =
  Result
    { name        :: T.Text
    , buildStatus :: BuildStatus
    , url         :: T.Text
    }
  deriving (Show)

instance TextShow Result where
  showb (Result n bs _) = showb n <> showbCommaSpace <> showb bs
