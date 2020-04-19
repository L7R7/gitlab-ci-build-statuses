{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib
  ( toMetricValue,
    updateStatuses,
    updateStatusesRegularly,
    BuildStatus (..),
    Result (..),
    GroupId (..),
  )
where

import Config hiding (apiToken, groupId)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (Result)
import Data.Either.Combinators (maybeToRight)
import Data.List
import qualified Data.Text as T hiding (partition)
import Env
import Katip
import Network.HTTP.Simple
import RIO hiding (logError, logInfo)
import TextShow
import Prelude hiding (id)

data UpdateError = HttpError HttpException | EmptyPipelinesResult | NoMasterRef deriving (Show)

updateStatusesRegularly :: (HasConfig env, HasStatuses env, KatipContext (RIO env)) => RIO env ()
updateStatusesRegularly =
  katipAddNamespace "update"
    $ forever
    $ do
      logLocM InfoS "updating build statuses"
      results <- updateStatuses
      katipAddContext (sl "numResults" $ show $ length results) $ logLocM InfoS "Done updating"
      cfg <- view configL
      threadDelay $ calculateDelay (dataUpdateIntervalMins cfg)

calculateDelay :: DataUpdateIntervalMinutes -> Int
calculateDelay (DataUpdateIntervalMinutes mins) = mins * 60 * oneSecond

oneSecond :: Int
oneSecond = 1000000

updateStatuses :: (HasConfig env, HasStatuses env, KatipContext (RIO env)) => RIO env [Result]
updateStatuses = do
  results <- currentKnownBuildStatuses
  ioref <- view statusesL
  liftIO $ atomicModifyIORef' ioref (const (results, results))

currentKnownBuildStatuses :: (HasConfig env, HasStatuses env, KatipContext (RIO env)) => RIO env [Result]
currentKnownBuildStatuses = filter (\r -> buildStatus r /= Unknown) <$> currentBuildStatuses

currentBuildStatuses :: (HasConfig env, HasStatuses env, KatipContext (RIO env)) => RIO env [Result]
currentBuildStatuses = do
  projects <- findProjects
  results <- traverse evalProject projects
  logCurrentBuildStatuses
  pure $ sortOn (T.toLower . name) results

logCurrentBuildStatuses :: (HasStatuses env, KatipContext (RIO env)) => RIO env ()
logCurrentBuildStatuses = do
  resultsIO <- view statusesL
  results <- readIORef resultsIO
  let (unknown, known) = partition (\r -> buildStatus r == Unknown) results
  if null known
    then logLocM WarningS "No valid Pipeline statuses found"
    else logLocM InfoS . ls $ "Pipeline status found for projects " <> concatIds known
  unless (null unknown) (logLocM InfoS . ls $ "No pipelines found for projects " <> concatIds unknown)
  where
    concatIds :: [Result] -> T.Text
    concatIds rs = T.intercalate ", " (showt . projId <$> rs)

evalProject :: (HasConfig env, KatipContext (RIO env)) => Project -> RIO env Result
evalProject (Project id name pUrl) = do
  buildStatusOrUpdateError <- findBuildStatus (ProjectId id)
  status <-
    case buildStatusOrUpdateError of
      Left EmptyPipelinesResult -> pure Unknown
      Left uError -> do
        logLocM InfoS . ls $ T.unwords ["Couldn't eval project with id", showt id, "- error was", (T.pack . show) uError]
        pure Unknown
      Right st -> pure $ toBuildStatus st
  pure $ Result id name status pUrl

findBuildStatus :: (HasConfig env, KatipContext (RIO env)) => ProjectId -> RIO env (Either UpdateError T.Text)
findBuildStatus id = do
  (Config _ _ baseUrl _ _) <- view configL
  pipelines <- fetchData $ pipelinesRequest baseUrl id
  pure $ pipelineStatus <$> (pipelines >>= pipelineForMaster)

findProjects :: (HasConfig env, KatipContext (RIO env)) => RIO env [Project]
findProjects = do
  (Config _ groupId baseUrl _ _) <- view configL
  result <- fetchData $ projectsRequest baseUrl groupId
  case result of
    Left err -> do
      logLocM InfoS . ls $ T.unwords ["Couldn't load projects. Error was", (T.pack . show) err]
      pure []
    Right ps -> pure ps

pipelineForMaster :: [Pipeline] -> Either UpdateError Pipeline
pipelineForMaster [] = Left EmptyPipelinesResult
pipelineForMaster pipelines = maybeToRight NoMasterRef (find (\p -> ref p == "master") pipelines)

fetchData :: (HasConfig env, FromJSON a) => Request -> RIO env (Either UpdateError [a])
fetchData request = do
  (Config (ApiToken apiToken) _ _ _ _) <- view configL
  result <- try (getResponseBody <$> httpJSON (setRequestHeader "PRIVATE-TOKEN" [apiToken] request))
  pure $ mapLeft HttpError result

projectsRequest :: BaseUrl -> GroupId -> Request
projectsRequest (BaseUrl baseUrl) (GroupId groupId) =
  parseRequest_ $ mconcat [baseUrl, "/api/v4/groups/", show groupId, "/projects?per_page=100&simple=true&include_subgroups=true"]

pipelinesRequest :: BaseUrl -> ProjectId -> Request
pipelinesRequest (BaseUrl baseUrl) (ProjectId i) = parseRequest_ $ mconcat [baseUrl, "/api/v4/projects/", show i, "/pipelines?scope=branches"]

data Project = Project {projectId :: Int, projectName :: T.Text, projectUrl :: T.Text} deriving (Show)

instance FromJSON Project where
  parseJSON = withObject "Project" $ \p -> Project <$> p .: "id" <*> p .: "name" <*> p .: "web_url"

data Pipeline = Pipeline {pipelineId :: Int, ref :: T.Text, pipelineStatus :: T.Text, pipelineUrl :: T.Text, pipelineRef :: T.Text} deriving (Show)

instance Eq Pipeline where
  (==) p1 p2 = pipelineId p1 == pipelineId p2

instance Ord Pipeline where
  (<=) p1 p2 = pipelineId p1 <= pipelineId p2

instance FromJSON Pipeline where
  parseJSON = withObject "Pipeline" $ \p -> Pipeline <$> p .: "id" <*> p .: "ref" <*> p .: "status" <*> p .: "web_url" <*> p .: "ref"

toBuildStatus :: T.Text -> BuildStatus
toBuildStatus "success" = Successful
toBuildStatus "running" = Running
toBuildStatus "failed" = Failed
toBuildStatus "canceled" = Cancelled
toBuildStatus "pending" = Pending
toBuildStatus "skipped" = Skipped
toBuildStatus _ = Unknown

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
