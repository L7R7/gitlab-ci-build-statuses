{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Core.Lib
  ( updateStatuses,
    updateStatusesRegularly,
    BuildStatus (..),
    BuildStatuses (..),
    DataUpdateIntervalMinutes (..),
    Result (..),
    GroupId (..),
    ProjectName (..),
    ProjectUrl (..),
    ProjectId (..),
    HasDataUpdateInterval (..),
    HasGetProjects (..),
    HasBuildStatuses (..),
    HasGetPipelines (..),
    UpdateError (..),
    Project (..),
    isHealthy,
  )
where

import Control.Monad
import Data.Aeson hiding (Result)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Coerce
import Data.Either.Combinators (maybeToRight)
import Data.List
import qualified Data.Text as T hiding (partition)
import Data.Time (UTCTime (..))
import Katip
import Network.HTTP.Simple (HttpException)
import Network.URI
import RIO hiding (id, logError, logInfo)
import Prelude hiding (id)

newtype GroupId = GroupId Int deriving newtype (Show)

data UpdateError = HttpError HttpException | EmptyPipelinesResult | NoPipelineForDefaultBranch deriving (Show)

newtype DataUpdateIntervalMinutes = DataUpdateIntervalMinutes Int deriving (Show)

class HasDataUpdateInterval env where
  dataUpdateIntervalL :: Lens' env DataUpdateIntervalMinutes

updateStatusesRegularly :: (HasGetProjects env, HasGetPipelines env, HasDataUpdateInterval env, HasBuildStatuses env, KatipContext (RIO env)) => RIO env ()
updateStatusesRegularly =
  katipAddNamespace "update" $
    forever $ do
      logLocM InfoS "updating build statuses"
      results <- updateStatuses
      katipAddContext (sl "numResults" $ show $ length results) $ logLocM InfoS "Done updating"
      updateInterval <- view dataUpdateIntervalL
      threadDelay $ calculateDelay updateInterval

calculateDelay :: DataUpdateIntervalMinutes -> Int
calculateDelay (DataUpdateIntervalMinutes mins) = mins * 60 * oneSecond

oneSecond :: Int
oneSecond = 1000000

updateStatuses :: (HasGetProjects env, HasGetPipelines env, HasBuildStatuses env, KatipContext (RIO env)) => RIO env [Result]
updateStatuses = do
  currentStatuses <- currentKnownBuildStatuses
  _ <- setStatuses currentStatuses
  pure currentStatuses

currentKnownBuildStatuses :: (HasGetProjects env, HasGetPipelines env, HasBuildStatuses env, KatipContext (RIO env)) => RIO env [Result]
currentKnownBuildStatuses = filter (\r -> buildStatus r /= Unknown) <$> currentBuildStatuses

currentBuildStatuses :: (HasGetProjects env, HasGetPipelines env, HasBuildStatuses env, KatipContext (RIO env)) => RIO env [Result]
currentBuildStatuses = do
  projects <- findProjects
  results <- traverse evalProject projects
  logCurrentBuildStatuses
  pure $ sortOn (T.toLower . coerce . name) results

logCurrentBuildStatuses :: (HasBuildStatuses env, KatipContext (RIO env)) => RIO env ()
logCurrentBuildStatuses = do
  statuses <- getStatuses
  logCurrentBuildStatuses' statuses

logCurrentBuildStatuses' :: (KatipContext (RIO env)) => BuildStatuses -> RIO env ()
logCurrentBuildStatuses' NoSuccessfulUpdateYet = logLocM InfoS "There was no successful update yet, so there are no pipeline statuses available"
logCurrentBuildStatuses' (Statuses statuses) = do
  let (unknown, known) = partition (\r -> buildStatus r == Unknown) (snd statuses)
  if null known
    then logLocM WarningS "No valid Pipeline statuses found"
    else katipAddContext (sl "projectIds" $ concatIds known) $ logLocM InfoS "Pipeline statuses found "
  unless (null unknown) (logLocM InfoS . ls $ "No pipelines found for projects " <> concatIds unknown)
  where
    concatIds :: [Result] -> T.Text
    concatIds rs = T.intercalate ", " (tshow . projId <$> rs)

evalProject :: (HasGetPipelines env, KatipContext (RIO env)) => Project -> RIO env Result
evalProject Project {..} = do
  pipelines <- getPipelines projectId
  let buildStatusOrUpdateError = pipelineStatus <$> (pipelines >>= pipelineForDefaultBranch projectDefaultBranch)
  status <- case buildStatusOrUpdateError of
    Left EmptyPipelinesResult -> pure Unknown
    Left uError -> do
      logLocM InfoS . ls $ T.unwords ["Couldn't eval project with id", tshow projectId, "- error was", tshow uError]
      pure Unknown
    Right st -> pure st
  pure $ Result projectId projectName status projectWebUrl

class HasGetPipelines env where
  getPipelines :: ProjectId -> RIO env (Either UpdateError [Pipeline])

class HasGetProjects env where
  getProjects :: RIO env (Either UpdateError [Project])

findProjects :: (HasGetProjects env, KatipContext (RIO env)) => RIO env [Project]
findProjects = do
  result <- getProjects
  case result of
    Left err -> do
      logLocM InfoS . ls $ T.unwords ["Couldn't load projects. Error was", tshow err]
      pure []
    Right ps -> pure ps

pipelineForDefaultBranch :: Ref -> [Pipeline] -> Either UpdateError Pipeline
pipelineForDefaultBranch _ [] = Left EmptyPipelinesResult
pipelineForDefaultBranch defaultBranch pipelines = maybeToRight NoPipelineForDefaultBranch (find (\p -> pipelineRef p == defaultBranch) pipelines)

data Project = Project {projectId :: ProjectId, projectName :: ProjectName, projectWebUrl :: ProjectUrl, projectDefaultBranch :: Ref} deriving (Generic, Show)

newtype ProjectName = ProjectName T.Text deriving (FromJSON, Show)

newtype ProjectId = ProjectId Int
  deriving (FromJSON)
  deriving newtype (Show)

newtype ProjectUrl = ProjectUrl URI deriving (FromJSON, Show)

instance FromJSON Project where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Pipeline = Pipeline {pipelineId :: PipelineId, pipelineRef :: Ref, pipelineStatus :: BuildStatus, pipelineWebUrl :: PipeLineUrl} deriving (Generic, Show)

newtype PipelineId = PipelineId Int deriving (Eq, FromJSON, Ord, Show)

newtype Ref = Ref T.Text deriving newtype (Eq, FromJSON, Show)

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
    "waiting_for_resource" -> pure WaitingForResource
    x -> fail $ mconcat ["couldn't parse build status from '", show x, "'"]

data Result = Result {projId :: ProjectId, name :: ProjectName, buildStatus :: BuildStatus, url :: ProjectUrl} deriving (Show)

data BuildStatus = Unknown | Running | Failed | Cancelled | Pending | Skipped | Successful | Created | Manual | WaitingForResource deriving (Eq, Show, Ord)

isHealthy :: BuildStatus -> Bool
isHealthy Successful = True
isHealthy Created = True
isHealthy WaitingForResource = True
isHealthy Pending = True
isHealthy Running = True
isHealthy _ = False

class HasBuildStatuses env where
  getStatuses :: RIO env BuildStatuses
  setStatuses :: [Result] -> RIO env BuildStatuses

data BuildStatuses = NoSuccessfulUpdateYet | Statuses (UTCTime, [Result])
