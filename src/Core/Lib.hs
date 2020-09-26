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
    BuildStatus (..),
    BuildStatuses (..),
    DataUpdateIntervalSeconds (..),
    MaxConcurrency (..),
    Result (..),
    Group,
    Id (..),
    Url (..),
    ProjectName (..),
    HasGetProjects (..),
    HasBuildStatuses (..),
    HasGetPipelines (..),
    UpdateError (..),
    DetailedPipeline,
    Pipeline,
    Project (..),
    Ref (..),
    isHealthy,
  )
where

import Data.Aeson hiding (Result)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Coerce
import Data.List
import qualified Data.Text as T hiding (partition)
import Data.Time (UTCTime (..))
import Katip
import Network.HTTP.Simple (HttpException, JSONException)
import Network.URI
import RIO hiding (id, logError, logInfo)

data Group

data UpdateError = HttpError HttpException | ConversionError JSONException | EmptyPipelinesResult | NoPipelineForDefaultBranch deriving (Show)

newtype DataUpdateIntervalSeconds = DataUpdateIntervalSeconds Int deriving (Show)

updateStatuses :: (HasGetProjects env, HasGetPipelines env, HasBuildStatuses env, KatipContext (RIO env)) => RIO env [Result]
updateStatuses = do
  currentStatuses <- currentKnownBuildStatuses
  _ <- setStatuses currentStatuses
  logCurrentBuildStatuses
  pure currentStatuses

currentKnownBuildStatuses :: (HasGetProjects env, HasGetPipelines env, KatipContext (RIO env)) => RIO env [Result]
currentKnownBuildStatuses = filter (\r -> buildStatus r /= Unknown) <$> currentBuildStatuses

currentBuildStatuses :: (HasGetProjects env, HasGetPipelines env, KatipContext (RIO env)) => RIO env [Result]
currentBuildStatuses = do
  projects <- findProjects
  (MaxConcurrency concurrency) <- view maxConcurrencyL
  results <- pooledMapConcurrentlyN concurrency evalProject projects
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
  result <- getStatusForProject projectId projectDefaultBranch
  pure $ case result of
    Nothing -> Result projectId projectName Unknown (Left projectWebUrl)
    Just (status, url) -> Result projectId projectName status (Right url)

getStatusForProject :: (HasGetPipelines env, KatipContext (RIO env)) => Id Project -> Maybe Ref -> RIO env (Maybe (BuildStatus, Url Pipeline))
getStatusForProject _ Nothing = pure Nothing
getStatusForProject projectId (Just defaultBranch) = do
  pipeline <- getLatestPipelineForRef projectId defaultBranch
  case pipeline of
    Left EmptyPipelinesResult -> pure Nothing
    Left NoPipelineForDefaultBranch -> pure Nothing
    Left uError -> do
      logLocM InfoS . ls $ T.unwords ["Couldn't eval project with id", tshow projectId, "- error was", tshow uError]
      pure Nothing
    Right p -> do
      let st = pipelineStatus p
      detailedStatus <-
        if st == Successful
          then detailedStatusForPipeline projectId (pipelineId p)
          else pure Nothing
      pure $ Just (fromMaybe st detailedStatus, pipelineWebUrl p)

class HasGetPipelines env where
  getLatestPipelineForRef :: Id Project -> Ref -> RIO env (Either UpdateError Pipeline)
  getSinglePipeline :: Id Project -> Id Pipeline -> RIO env (Either UpdateError DetailedPipeline)

data DetailedPipeline = DetailedPipeline {detailedPipelineId :: Id Pipeline, detailedPipelineRef :: Ref, detailedPipelineStatus :: BuildStatus, detailedPipelineWebUrl :: Url Pipeline}

instance FromJSON DetailedPipeline where
  parseJSON = withObject "detailedPipeline" $ \dp -> do
    detailedPipelineId <- dp .: "id"
    detailedPipelineRef <- dp .: "ref"
    detailedPipelineWebUrl <- dp .: "web_url"
    detailedPipelineStatus <- dp .: "detailed_status" >>= \ds -> ds .: "group"
    pure DetailedPipeline {..}

class HasGetProjects env where
  getProjects :: RIO env (Either UpdateError [Project])
  maxConcurrencyL :: SimpleGetter env MaxConcurrency

newtype MaxConcurrency = MaxConcurrency Int deriving (Show)

findProjects :: (HasGetProjects env, KatipContext (RIO env)) => RIO env [Project]
findProjects = do
  result <- getProjects
  case result of
    Left err -> do
      logLocM InfoS . ls $ T.unwords ["Couldn't load projects. Error was", tshow err]
      pure []
    Right ps -> pure ps

detailedStatusForPipeline :: (HasGetPipelines env, KatipContext (RIO env)) => Id Project -> Id Pipeline -> RIO env (Maybe BuildStatus)
detailedStatusForPipeline projectId pipelineId = katipAddContext (sl "projectId" projectId <> sl "pipelineId" pipelineId) $ do
  singlePipelineResult <- getSinglePipeline projectId pipelineId
  case singlePipelineResult of
    Left uError -> do
      logLocM WarningS . ls $ T.unwords ["Couldn't get details for pipeline, error was", tshow uError]
      pure Nothing
    Right dp -> pure . Just $ detailedPipelineStatus dp

data Project = Project {projectId :: Id Project, projectName :: ProjectName, projectWebUrl :: Url Project, projectDefaultBranch :: Maybe Ref} deriving (Generic, Show)

newtype ProjectName = ProjectName T.Text deriving (FromJSON, Show)

instance FromJSON Project where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Pipeline = Pipeline {pipelineId :: Id Pipeline, pipelineRef :: Ref, pipelineStatus :: BuildStatus, pipelineWebUrl :: Url Pipeline} deriving (Generic, Show)

newtype Id a = Id Int deriving newtype (Eq, FromJSON, Ord, Show, ToJSON)

newtype Url a = Url URI deriving (Show)

instance FromJSON (Url a) where
  parseJSON = withText "URI" $ \v -> Url <$> maybe (fail "Bad URI") pure (parseURI (T.unpack v))

newtype Ref = Ref T.Text deriving newtype (Eq, FromJSON, Show)

instance Eq Pipeline where
  (==) p p' = pipelineId p == pipelineId p'

instance Ord Pipeline where
  (<=) p p' = pipelineId p <= pipelineId p'

instance FromJSON Pipeline where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

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
    "success-with-warnings" -> pure SuccessfulWithWarnings
    "preparing" -> pure Preparing
    "scheduled" -> pure Scheduled
    x -> fail $ mconcat ["couldn't parse build status from '", show x, "'"]

data Result = Result {projId :: Id Project, name :: ProjectName, buildStatus :: BuildStatus, url :: Either (Url Project) (Url Pipeline)} deriving (Show)

data BuildStatus
  = Unknown
  | Running
  | Failed
  | Cancelled
  | Pending
  | Skipped
  | Successful
  | Created
  | Manual
  | WaitingForResource
  | SuccessfulWithWarnings
  | Preparing
  | Scheduled
  deriving (Bounded, Enum, Eq, Show, Ord)

isHealthy :: BuildStatus -> Bool
isHealthy Successful = True
isHealthy Created = True
isHealthy WaitingForResource = True
isHealthy Pending = True
isHealthy Running = True
isHealthy Unknown = False
isHealthy Failed = False
isHealthy Cancelled = False
isHealthy Skipped = False
isHealthy Manual = False
isHealthy SuccessfulWithWarnings = False
isHealthy Preparing = True
isHealthy Scheduled = True

class HasBuildStatuses env where
  getStatuses :: RIO env BuildStatuses
  setStatuses :: [Result] -> RIO env BuildStatuses

data BuildStatuses = NoSuccessfulUpdateYet | Statuses (UTCTime, [Result])
