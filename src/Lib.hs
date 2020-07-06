{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib
  ( toMetricValue,
    updateStatuses,
    updateStatusesRegularly,
    BuildStatus (..),
    Result (..),
    GroupId (..),
    ProjectName (..),
    ProjectUrl (..),
  )
where

import Config hiding (apiToken, groupId)
import Control.Monad
import Data.Aeson hiding (Result)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Coerce
import Data.Either.Combinators (maybeToRight)
import Data.List
import qualified Data.Text as T hiding (partition)
import Data.Time (getCurrentTime)
import Env
import Katip
import Network.HTTP.Simple
import Network.URI
import RIO hiding (logError, logInfo)
import Prelude hiding (id)

data UpdateError = HttpError HttpException | EmptyPipelinesResult | NoMasterRef deriving (Show)

updateStatusesRegularly :: (HasApiToken env, HasBaseUrl env, HasGroupId env, HasDataUpdateInterval env, HasStatuses env Result, KatipContext (RIO env)) => RIO env ()
updateStatusesRegularly =
  katipAddNamespace "update"
    $ forever
    $ do
      logLocM InfoS "updating build statuses"
      results <- updateStatuses
      katipAddContext (sl "numResults" $ show $ length results) $ logLocM InfoS "Done updating"
      dataUpdateInterval <- view dataUpdateIntervalL
      threadDelay $ calculateDelay dataUpdateInterval

calculateDelay :: DataUpdateIntervalMinutes -> Int
calculateDelay (DataUpdateIntervalMinutes mins) = mins * 60 * oneSecond

oneSecond :: Int
oneSecond = 1000000

updateStatuses :: (HasApiToken env, HasBaseUrl env, HasGroupId env, HasStatuses env Result, KatipContext (RIO env)) => RIO env [Result]
updateStatuses = do
  results <- currentKnownBuildStatuses
  updateTime <- liftIO getCurrentTime
  ioref <- view statusesL
  liftIO $ atomicModifyIORef' ioref (const ((Just updateTime, results), results))

currentKnownBuildStatuses :: (HasApiToken env, HasBaseUrl env, HasGroupId env, HasStatuses env Result, KatipContext (RIO env)) => RIO env [Result]
currentKnownBuildStatuses = filter (\r -> buildStatus r /= Unknown) <$> currentBuildStatuses

currentBuildStatuses :: (HasApiToken env, HasBaseUrl env, HasGroupId env, HasStatuses env Result, KatipContext (RIO env)) => RIO env [Result]
currentBuildStatuses = do
  projects <- findProjects
  results <- traverse evalProject projects
  logCurrentBuildStatuses
  pure $ sortOn (T.toLower . coerce . name) results

logCurrentBuildStatuses :: (HasStatuses env Result, KatipContext (RIO env)) => RIO env ()
logCurrentBuildStatuses = do
  resultsIO <- view statusesL
  results <- readIORef resultsIO
  let (unknown, known) = partition (\r -> buildStatus r == Unknown) (snd results)
  if null known
    then logLocM WarningS "No valid Pipeline statuses found"
    else katipAddContext (sl "projectIds" $ concatIds known) $ logLocM InfoS "Pipeline statuses found "
  unless (null unknown) (logLocM InfoS . ls $ "No pipelines found for projects " <> concatIds unknown)
  where
    concatIds :: [Result] -> T.Text
    concatIds rs = T.intercalate ", " (tshow . projId <$> rs)

evalProject :: (HasApiToken env, HasBaseUrl env, KatipContext (RIO env)) => Project -> RIO env Result
evalProject (Project id name pUrl) = do
  buildStatusOrUpdateError <- findBuildStatus id
  status <- case buildStatusOrUpdateError of
    Left EmptyPipelinesResult -> pure Unknown
    Left uError -> do
      logLocM InfoS . ls $ T.unwords ["Couldn't eval project with id", tshow id, "- error was", tshow uError]
      pure Unknown
    Right st -> pure st
  pure $ Result id name status pUrl

findBuildStatus :: (HasApiToken env, HasBaseUrl env, KatipContext (RIO env)) => ProjectId -> RIO env (Either UpdateError BuildStatus)
findBuildStatus id = do
  baseUrl <- view baseUrlL
  pipelines <- fetchData $ pipelinesRequest baseUrl id
  pure $ pipelineStatus <$> (pipelines >>= pipelineForMaster)

findProjects :: (HasApiToken env, HasBaseUrl env, HasGroupId env, KatipContext (RIO env)) => RIO env [Project]
findProjects = do
  baseUrl <- view baseUrlL
  groupId <- view groupIdL
  result <- fetchData $ projectsRequest baseUrl groupId
  case result of
    Left err -> do
      logLocM InfoS . ls $ T.unwords ["Couldn't load projects. Error was", tshow err]
      pure []
    Right ps -> pure ps

pipelineForMaster :: [Pipeline] -> Either UpdateError Pipeline
pipelineForMaster [] = Left EmptyPipelinesResult
pipelineForMaster pipelines = maybeToRight NoMasterRef (find (\p -> pipelineRef p == Master) pipelines)

fetchData :: (HasApiToken env, FromJSON a) => Request -> RIO env (Either UpdateError [a])
fetchData request = do
  (ApiToken apiToken) <- view apiTokenL
  result <- try (getResponseBody <$> httpJSON (setRequestHeader "PRIVATE-TOKEN" [apiToken] request))
  pure $ mapLeft HttpError result

projectsRequest :: BaseUrl -> GroupId -> Request
projectsRequest (BaseUrl baseUrl) (GroupId groupId) =
  parseRequest_ $ mconcat [baseUrl, "/api/v4/groups/", show groupId, "/projects?per_page=100&simple=true&include_subgroups=true"]

pipelinesRequest :: BaseUrl -> ProjectId -> Request
pipelinesRequest (BaseUrl baseUrl) (ProjectId i) = parseRequest_ $ mconcat [baseUrl, "/api/v4/projects/", show i, "/pipelines?scope=branches"]

data Project = Project {projectId :: ProjectId, projectName :: ProjectName, projectWebUrl :: ProjectUrl} deriving (Generic, Show)

newtype ProjectName = ProjectName T.Text deriving (FromJSON, Show)

newtype ProjectId = ProjectId Int deriving (FromJSON, Show)

newtype ProjectUrl = ProjectUrl URI deriving (FromJSON, Show)

instance FromJSON Project where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Pipeline = Pipeline {pipelineId :: PipelineId, pipelineRef :: Ref, pipelineStatus :: BuildStatus, pipelineWebUrl :: PipeLineUrl} deriving (Generic, Show)

newtype PipelineId = PipelineId Int deriving (Eq, FromJSON, Ord, Show)

data Ref = Master | Ref T.Text deriving (Eq)

instance Show Ref where
  show Master = "master"
  show (Ref r) = show r

instance FromJSON Ref where
  parseJSON = withText "Ref" $ \case
    "master" -> pure Master
    t -> (pure . Ref) t

newtype PipeLineUrl = PipeLineUrl URI deriving (FromJSON, Show)

instance Eq Pipeline where
  (==) p p' = pipelineId p == pipelineId p'

instance Ord Pipeline where
  (<=) p p' = pipelineId p <= pipelineId p'

instance FromJSON Pipeline where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON URI where
  parseJSON = withText "URI" $ \v -> maybe (fail "Bad URI") pure (parseURI (T.unpack v))

instance FromJSON BuildStatus where
  parseJSON = withText "BuildStatus" $ \case
    "running" -> pure Running
    "pending" -> pure Pending
    "success" -> pure Successful
    "failed" -> pure Failed
    "canceled" -> pure Cancelled
    "skipped" -> pure Skipped
    "created" -> pure Created
    "manual" -> pure Manual

-- | map a build status to a float value that's suitable for displaying it on Grafana dashboard.
-- Suggested colors are:
-- 0 -> green
-- 1 -> red
-- >1 -> blue
toMetricValue :: BuildStatus -> Float
toMetricValue Successful = 0
toMetricValue Failed = 1
toMetricValue Unknown = 2
toMetricValue Running = 3
toMetricValue Cancelled = 4
toMetricValue Pending = 5
toMetricValue Skipped = 6

data Result = Result {projId :: ProjectId, name :: ProjectName, buildStatus :: BuildStatus, url :: ProjectUrl} deriving (Show)

data BuildStatus = Unknown | Running | Failed | Cancelled | Pending | Skipped | Successful | Created | Manual deriving (Eq, Show, Ord)
