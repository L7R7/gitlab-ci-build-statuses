{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Core.BuildStatuses
  ( BuildStatus (..),
    BuildStatuses (..),
    Result (..),
    ProjectsApi (..),
    getProjects,
    BuildStatusesApi (..),
    getStatuses,
    setStatuses,
    PipelinesApi (..),
    getLatestPipelineForRef,
    getSinglePipeline,
    DetailedPipeline (..),
    Pipeline (..),
    Project (..),
    isHealthy,
    toResult,
  )
where

import Core.Shared
import Data.Time (UTCTime (..))
import Path
import Polysemy
import Relude

data Pipeline = Pipeline
  { pipelineId :: Id Pipeline,
    pipelineRef :: Ref,
    pipelineStatus :: BuildStatus,
    pipelineWebUrl :: Url Pipeline
  }
  deriving stock (Generic, Show)

instance Eq Pipeline where
  p == p' = pipelineId p == pipelineId p'

instance Ord Pipeline where
  p <= p' = pipelineId p <= pipelineId p'

data Result = Result
  { projId :: Id Project,
    name :: Name Project,
    buildStatus :: BuildStatus,
    url :: Either (Url Project) (Url Pipeline)
  }
  deriving stock (Eq, Show)

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
  deriving stock (Bounded, Enum, Eq, Show, Ord)

data BuildStatuses = NoSuccessfulUpdateYet | Statuses (UTCTime, [Result])

data DetailedPipeline = DetailedPipeline
  { detailedPipelineId :: Id Pipeline,
    detailedPipelineRef :: Ref,
    detailedPipelineStatus :: BuildStatus,
    detailedPipelineWebUrl :: Url Pipeline
  }

data Project = Project
  { projectId :: Id Project,
    projectName :: Name Project,
    projectWebUrl :: Url Project,
    projectDefaultBranch :: Maybe Ref,
    projectNamespacePath :: Path Rel Dir
  }
  deriving stock (Generic, Show)

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

toResult :: Project -> Maybe (BuildStatus, Url Pipeline) -> Result
toResult Project {..} Nothing = Result projectId projectName Unknown (Left projectWebUrl)
toResult Project {..} (Just (status, url)) = Result projectId projectName status (Right url)
