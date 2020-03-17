{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( toMetricValue
  , updateStatuses
  , updateStatusesRegularly
  , BuildStatus(..)
  , Result(..)
  , GroupId(..)
  ) where

import           Config                  hiding (apiToken, groupId)
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Aeson              hiding (Result)
import           Data.Either.Combinators
import           Data.IORef
import           Data.List
import qualified Data.Text               as T hiding (partition)
import qualified Data.Text.IO            as TIO
import           Data.Time
import           Network.HTTP.Simple
import           Prelude                 hiding (id)
import           TextShow

data UpdateError
  = HttpError HttpException
  | EmptyPipelinesResult
  deriving (Show)

updateStatusesRegularly :: Config -> IORef [Result] -> IO ()
updateStatusesRegularly config ioref =
  forever $ do
    putStrLn "updating build statuses"
    results <- updateStatuses config ioref
    currentTime <- getCurrentTime
    putStrLn $ unwords ["Done updating.", show $ length results, "results. Update finished at", show currentTime]
    threadDelay $ calculateDelay (dataUpdateIntervalMins config)

calculateDelay :: DataUpdateIntervalMinutes -> Int
calculateDelay (DataUpdateIntervalMinutes mins) = mins * 60 * 1000000

updateStatuses :: Config -> IORef [Result] -> IO [Result]
updateStatuses config ioref = do
  results <- currentKnownBuildStatuses config
  atomicModifyIORef' ioref (const (results, results))

currentKnownBuildStatuses :: Config -> IO [Result]
currentKnownBuildStatuses config = do
  statuses <- currentBuildStatuses config
  pure $ filter (\r -> buildStatus r /= Unknown) statuses

currentBuildStatuses :: Config -> IO [Result]
currentBuildStatuses (Config apiToken groupId baseUrl _ _) = do
  projects <- findProjects apiToken baseUrl groupId
  statuses <- traverse (evalProject apiToken baseUrl) projects
  pure $ sortOn (T.toLower . name) statuses

evalProject :: ApiToken -> BaseUrl -> Project -> IO Result
evalProject apiToken baseUrl (Project id name pUrl) = do
  TIO.putStrLn $ T.unwords ["Getting build status for project", showt id, "-", name]
  maybeBuildStatus <- findBuildStatus apiToken baseUrl (ProjectId id)
  status <-
    case maybeBuildStatus of
      Left EmptyPipelinesResult -> do
        putStrLn $ unwords ["No pipelines found for project", show id]
        pure Unknown
      Left uError -> do
        putStrLn $ unwords ["Couldn't eval project with id", show id, "- error was", show uError]
        pure Unknown
      Right st -> pure $ toBuildStatus st
  pure $ Result name status pUrl

findBuildStatus :: ApiToken -> BaseUrl -> ProjectId -> IO (Either UpdateError T.Text)
findBuildStatus apiToken baseUrl id = do
  pipelines <- fetchData apiToken $ pipelinesRequest baseUrl id
  pure $ pipelineStatus <$> (pipelines >>= maxByPipelineId)

findProjects :: ApiToken -> BaseUrl -> GroupId -> IO [Project]
findProjects apiToken baseUrl groupId = do
  result <- fetchData apiToken $ projectsRequest baseUrl groupId
  case result of
    Left err -> do
      putStrLn $ unwords ["Couldn't load projects. Error was", show err]
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
