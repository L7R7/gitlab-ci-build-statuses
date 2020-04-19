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

import Colog
import Config hiding (apiToken, groupId)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (Result)
import Data.List
import qualified Data.Text as T hiding (partition)
import Network.HTTP.Simple
import RIO hiding (logError, logInfo)
import TextShow
import Prelude hiding (id)
import Data.Either.Combinators (maybeToRight)

data UpdateError = HttpError HttpException | EmptyPipelinesResult | NoMasterRef deriving (Show)

updateStatusesRegularly :: Config -> IORef [Result] -> LoggerT Message IO ()
updateStatusesRegularly config ioref =
  forever $ do
    logInfo "updating build statuses"
    results <- updateStatuses config ioref
    logInfo $ T.unwords ["Done updating.", showt $ length results, "results"]
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
  results <- traverse (evalProject apiToken baseUrl) projects
  logCurrentBuildStatuses results
  pure $ sortOn (T.toLower . name) results

logCurrentBuildStatuses :: [Result] -> LoggerT Message IO ()
logCurrentBuildStatuses results = do
  let (unknown, known) = partition (\r -> buildStatus r == Unknown) results
  if null known
    then logWarning "No valid Pipeline statuses found"
    else logInfo $ "Pipeline status found for projects " <> concatIds known
  unless (null unknown) (logInfo $ "No pipelines found for projects " <> concatIds unknown)
  where
    concatIds :: [Result] -> T.Text
    concatIds rs = T.intercalate ", " (showt . projId <$> rs)

evalProject :: ApiToken -> BaseUrl -> Project -> LoggerT Message IO Result
evalProject apiToken baseUrl (Project id name pUrl) = do
  buildStatusOrUpdateError <- findBuildStatus apiToken baseUrl (ProjectId id)
  status <-
    case buildStatusOrUpdateError of
      Left EmptyPipelinesResult -> pure Unknown
      Left uError -> do
        logInfo $ T.unwords ["Couldn't eval project with id", showt id, "- error was", (T.pack . show) uError]
        pure Unknown
      Right st -> pure $ toBuildStatus st
  pure $ Result id name status pUrl

findBuildStatus :: ApiToken -> BaseUrl -> ProjectId -> LoggerT Message IO (Either UpdateError T.Text)
findBuildStatus apiToken baseUrl id = do
  pipelines <- liftIO $ fetchData apiToken $ pipelinesRequest baseUrl id
  pure $ pipelineStatus <$> (pipelines >>= pipelineForMaster)

findProjects :: ApiToken -> BaseUrl -> GroupId -> LoggerT Message IO [Project]
findProjects apiToken baseUrl groupId = do
  result <- liftIO $ fetchData apiToken $ projectsRequest baseUrl groupId
  case result of
    Left err -> do
      logInfo $ T.unwords ["Couldn't load projects. Error was", (T.pack . show) err]
      pure []
    Right ps -> pure ps

pipelineForMaster :: [Pipeline] -> Either UpdateError Pipeline
pipelineForMaster [] = Left EmptyPipelinesResult
pipelineForMaster pipelines = maybeToRight NoMasterRef (find (\p -> ref p == "master") pipelines)

fetchData :: FromJSON a => ApiToken -> Request -> IO (Either UpdateError [a])
fetchData (ApiToken apiToken) request = do
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

data BuildStatus = Unknown | Running | Failed | Cancelled | Pending | Skipped | Successful deriving (Eq, Show, Ord)

instance TextShow BuildStatus where
  showb = showb . show

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

data Result = Result {projId :: Int, name :: T.Text, buildStatus :: BuildStatus, url :: T.Text} deriving (Show)

instance TextShow Result where
  showb (Result i n bs _) = showb i <> showbCommaSpace <> showb n <> showbCommaSpace <> showb bs
