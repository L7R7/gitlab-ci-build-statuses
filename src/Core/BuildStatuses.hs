{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Core.BuildStatuses
  ( BuildStatuses (..),
    filterResults,
    Result (..),
    ProjectsApi (..),
    getProject,
    getProjects,
    ProjectsWithoutExcludesApi (..),
    getProjectsNotOnExcludeListOrEmpty,
    BuildStatusesApi (..),
    getStatuses,
    setStatuses,
    PipelinesApi (..),
    getLatestPipelineForRef,
    getSinglePipeline,
    DetailedPipeline (..),
    Pipeline (..),
    isHealthy,
    toResult,
  )
where

import Core.Shared (UpdateError)
import Data.Time (UTCTime (..))
import Gitlab.Group
import Gitlab.Job (JobStatus (..))
import Gitlab.Lib (Id, Name, Ref, Url)
import Gitlab.Project (Project (..), ProjectNamespace (..), ProjectNamespaceFullPath)
import Polysemy
import Relude

data Pipeline = Pipeline
  { pipelineId :: Id Pipeline,
    pipelineRef :: Ref,
    pipelineStatus :: JobStatus,
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
    namespace :: ProjectNamespaceFullPath,
    buildStatus :: JobStatus,
    url :: Either (Url Project) (Url Pipeline)
  }
  deriving stock (Eq, Show)

data BuildStatuses = NoSuccessfulUpdateYet | Statuses (UTCTime, [Result])

filterResults :: BuildStatuses -> (Result -> Bool) -> BuildStatuses
filterResults NoSuccessfulUpdateYet _ = NoSuccessfulUpdateYet
filterResults (Statuses (t, res)) f = Statuses (t, filter f res)

data DetailedPipeline = DetailedPipeline
  { detailedPipelineId :: Id Pipeline,
    detailedPipelineRef :: Ref,
    detailedPipelineStatus :: JobStatus,
    detailedPipelineWebUrl :: Url Pipeline
  }

-- todo: is this right?
instance Ord Project where
  p1 <= p2 = projectId p1 <= projectId p2

data PipelinesApi m a where
  GetLatestPipelineForRef :: Id Project -> Ref -> PipelinesApi m (Either UpdateError Pipeline)
  GetSinglePipeline :: Id Project -> Id Pipeline -> PipelinesApi m (Either UpdateError DetailedPipeline)

makeSem ''PipelinesApi

data ProjectsApi m a where
  GetProject :: Id Project -> ProjectsApi m (Either UpdateError Project)
  GetProjects :: Id Group -> ProjectsApi m (Either UpdateError [Project])

makeSem ''ProjectsApi

data ProjectsWithoutExcludesApi m a where
  GetProjectsNotOnExcludeListOrEmpty :: Id Group -> ProjectsWithoutExcludesApi m [Project]

makeSem ''ProjectsWithoutExcludesApi

data BuildStatusesApi m a where
  GetStatuses :: BuildStatusesApi m BuildStatuses
  SetStatuses :: [Result] -> BuildStatusesApi m ()

makeSem ''BuildStatusesApi

isHealthy :: JobStatus -> Bool
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

toResult :: Project -> Maybe (JobStatus, Url Pipeline) -> Result
toResult Project {..} Nothing = Result projectId projectName (projectNamespaceFullPath projectNamespace) Unknown (Left projectWebUrl)
toResult Project {..} (Just (status, url)) = Result projectId projectName (projectNamespaceFullPath projectNamespace) status (Right url)
