{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Core.Lib
  ( updateStatuses,
    currentBuildStatuses,
    BuildStatus (..),
    BuildStatuses (..),
    DataUpdateIntervalSeconds (..),
    Result (..),
    Group,
    Id (..),
    Url (..),
    ProjectName (..),
    ProjectsApi (..),
    BuildStatusesApi (..),
    getStatuses,
    getStatusForProject,
    PipelinesApi (..),
    UpdateError (..),
    DetailedPipeline (..),
    Pipeline (..),
    Project (..),
    Ref (..),
    isHealthy,
  )
where

import Core.Effects (Logger, ParTraverse, addContext, logDebug, logWarn, traverseP)
import Data.Aeson hiding (Result)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.List (partition)
import qualified Data.Text as T (intercalate, toLower)
import Data.Time (UTCTime (..))
import Network.HTTP.Simple (HttpException, JSONException)
import Network.URI
import Polysemy
import Relude

data Group

data UpdateError
  = HttpError HttpException
  | ConversionError JSONException
  | EmptyPipelinesResult
  | NoPipelineForDefaultBranch
  deriving (Show)

newtype DataUpdateIntervalSeconds = DataUpdateIntervalSeconds Int deriving (Show)

data Pipeline = Pipeline
  { pipelineId :: Id Pipeline,
    pipelineRef :: Ref,
    pipelineStatus :: BuildStatus,
    pipelineWebUrl :: Url Pipeline
  }
  deriving (Generic, Show)

newtype Id a = Id Int deriving newtype (Eq, FromJSON, Hashable, Ord, Show, ToJSON)

newtype Url a = Url URI deriving newtype (Eq, Show)

instance FromJSON (Url a) where
  parseJSON = withText "URI" $ \v -> Url <$> maybe (fail "Bad URI") pure (parseURI (toString v))

newtype Ref = Ref Text deriving newtype (Eq, FromJSON, Ord, Show)

instance Eq Pipeline where
  p == p' = pipelineId p == pipelineId p'

instance Ord Pipeline where
  p <= p' = pipelineId p <= pipelineId p'

instance FromJSON Pipeline where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Result = Result
  { projId :: Id Project,
    name :: ProjectName,
    buildStatus :: BuildStatus,
    url :: Either (Url Project) (Url Pipeline)
  }
  deriving (Eq, Show)

data BuildStatus
  = Unknown
  | Cancelled
  | Created
  | Failed
  | Manual
  | Pending
  | Preparing
  | Running
  | Scheduled
  | Skipped
  | Successful
  | SuccessfulWithWarnings
  | WaitingForResource
  deriving (Bounded, Enum, Eq, Show, Ord)

instance FromJSON BuildStatus where
  parseJSON = withText "BuildStatus" $ \x -> maybe (fail $ mconcat ["couldn't parse build status from '", show x, "'"]) pure (inverseMap buildStatusToApiString x)

buildStatusToApiString :: IsString p => BuildStatus -> p
buildStatusToApiString Unknown = "unknown"
buildStatusToApiString Cancelled = "canceled"
buildStatusToApiString Created = "created"
buildStatusToApiString Failed = "failed"
buildStatusToApiString Manual = "manual"
buildStatusToApiString Pending = "pending"
buildStatusToApiString Preparing = "preparing"
buildStatusToApiString Running = "running"
buildStatusToApiString Scheduled = "scheduled"
buildStatusToApiString Skipped = "skipped"
buildStatusToApiString Successful = "success"
buildStatusToApiString SuccessfulWithWarnings = "success-with-warnings"
buildStatusToApiString WaitingForResource = "waiting_for_resource"

data BuildStatuses = NoSuccessfulUpdateYet | Statuses (UTCTime, [Result])

data DetailedPipeline = DetailedPipeline
  { detailedPipelineId :: Id Pipeline,
    detailedPipelineRef :: Ref,
    detailedPipelineStatus :: BuildStatus,
    detailedPipelineWebUrl :: Url Pipeline
  }

instance FromJSON DetailedPipeline where
  parseJSON = withObject "detailedPipeline" $ \dp -> do
    detailedPipelineId <- dp .: "id"
    detailedPipelineRef <- dp .: "ref"
    detailedPipelineWebUrl <- dp .: "web_url"
    detailedPipelineStatus <- dp .: "detailed_status" >>= \ds -> ds .: "group"
    pure DetailedPipeline {..}

data Project = Project
  { projectId :: Id Project,
    projectName :: ProjectName,
    projectWebUrl :: Url Project,
    projectDefaultBranch :: Maybe Ref
  }
  deriving (Generic, Show)

newtype ProjectName = ProjectName Text deriving (Eq, FromJSON, Show)

instance FromJSON Project where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data PipelinesApi m a where
  GetLatestPipelineForRef :: Id Project -> Ref -> PipelinesApi m (Either UpdateError Pipeline)
  GetSinglePipeline :: Id Project -> Id Pipeline -> PipelinesApi m (Either UpdateError DetailedPipeline)

makeSem ''PipelinesApi

data ProjectsApi m a where
  GetProjects :: Id Group -> ProjectsApi m (Either UpdateError [Project])

makeSem ''ProjectsApi

data BuildStatusesApi m a where
  GetStatuses :: BuildStatusesApi m BuildStatuses
  SetStatuses :: [Result] -> BuildStatusesApi m ()

makeSem ''BuildStatusesApi

updateStatuses :: (Member ProjectsApi r, Member PipelinesApi r, Member BuildStatusesApi r, Member Logger r, Member ParTraverse r) => Id Group -> Sem r [Result]
updateStatuses groupId = do
  currentStatuses <- currentKnownBuildStatuses groupId
  unless (null currentStatuses) $ setStatuses currentStatuses
  logCurrentBuildStatuses
  pure currentStatuses

currentKnownBuildStatuses :: (Member ProjectsApi r, Member PipelinesApi r, Member Logger r, Member ParTraverse r) => Id Group -> Sem r [Result]
currentKnownBuildStatuses groupId = filter ((/= Unknown) . buildStatus) <$> currentBuildStatuses groupId

currentBuildStatuses :: (Member ParTraverse r, Member ProjectsApi r, Member PipelinesApi r, Member Logger r) => Id Group -> Sem r [Result]
currentBuildStatuses groupId = do
  projects <- findProjects groupId
  results <- traverseP evalProject projects
  pure $ sortOn (T.toLower . coerce . name) results

findProjects :: (Member ProjectsApi r, Member Logger r) => Id Group -> Sem r [Project]
findProjects groupId = do
  result <- getProjects groupId
  case result of
    Left err -> [] <$ logWarn (unwords ["Couldn't load projects. Error was", show err])
    Right ps -> pure ps

logCurrentBuildStatuses :: (Member BuildStatusesApi r, Member Logger r) => Sem r ()
logCurrentBuildStatuses = do
  result <- getStatuses
  case result of
    NoSuccessfulUpdateYet -> logDebug "There was no successful update yet, so there are no pipeline statuses available"
    (Statuses (_, statuses)) -> do
      let (unknown, known) = partition ((== Unknown) . buildStatus) statuses
      if null known
        then logWarn "No valid Pipeline statuses found"
        else addContext "projectIds" (concatIds known) $ logDebug "Pipeline statuses found"
      unless (null unknown) (logDebug $ "No pipelines found for projects " <> concatIds unknown)
  where
    concatIds :: [Result] -> Text
    concatIds rs = T.intercalate ", " (show . projId <$> rs)

evalProject :: (Member PipelinesApi r, Member Logger r) => Project -> Sem r Result
evalProject Project {..} = evalProject' <$> getStatusForProject projectId projectDefaultBranch
  where
    evalProject' :: Maybe (BuildStatus, Url Pipeline) -> Result
    evalProject' Nothing = Result projectId projectName Unknown (Left projectWebUrl)
    evalProject' (Just (status, url)) = Result projectId projectName status (Right url)

getStatusForProject :: (Member PipelinesApi r, Member Logger r) => Id Project -> Maybe Ref -> Sem r (Maybe (BuildStatus, Url Pipeline))
getStatusForProject _ Nothing = pure Nothing
getStatusForProject projectId (Just defaultBranch) = addContext "projectId" projectId $ do
  pipeline <- getLatestPipelineForRef projectId defaultBranch
  case pipeline of
    Left EmptyPipelinesResult -> pure Nothing
    Left NoPipelineForDefaultBranch -> pure Nothing
    Left uError -> Nothing <$ logWarn (unwords ["Couldn't eval project. Error was", show uError])
    Right p -> do
      let st = pipelineStatus p
      detailedStatus <- if st == Successful then detailedStatusForPipeline projectId (pipelineId p) else pure Nothing
      pure $ Just (fromMaybe st detailedStatus, pipelineWebUrl p)

detailedStatusForPipeline :: (Member PipelinesApi r, Member Logger r) => Id Project -> Id Pipeline -> Sem r (Maybe BuildStatus)
detailedStatusForPipeline projectId pipelineId =
  addContext "pipelineId" pipelineId $ do
    singlePipelineResult <- getSinglePipeline projectId pipelineId
    case singlePipelineResult of
      Left uError -> Nothing <$ logWarn (unwords ["Couldn't get details for pipeline, error was", show uError])
      Right dp -> pure . Just $ detailedPipelineStatus dp

isHealthy :: BuildStatus -> Bool
isHealthy Unknown = False
isHealthy Cancelled = False
isHealthy Created = True
isHealthy Failed = False
isHealthy Manual = False
isHealthy Pending = True
isHealthy Preparing = True
isHealthy Running = True
isHealthy Scheduled = True
isHealthy Skipped = False
isHealthy Successful = True
isHealthy SuccessfulWithWarnings = False
isHealthy WaitingForResource = True
