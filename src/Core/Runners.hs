{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Core.Runners
  ( Runner (..),
    Description (..),
    Tag (..),
    Job (..),
    Stage (..),
    IpAddress (..),
    RunnersJobs (..),
    RunnersApi (..),
    getOnlineRunnersForGroup,
    getProjectRunnersForGroup,
    getRunningJobsForRunner,
    RunnersJobsApi (..),
    getJobs,
    setJobs,
  )
where

import Core.BuildStatuses (Project)
import Core.Shared
import Data.Time (UTCTime)
import Polysemy (makeSem)
import Relude

data Runner = Runner
  { runnerId :: Id Runner,
    runnerName :: Maybe (Name Runner),
    runnerDescription :: Description,
    runnerIpAddress :: Maybe IpAddress,
    runnerTagList :: [Tag]
  }
  deriving stock (Eq, Generic)

instance Ord Runner where
  compare r1 r2 = compare (runnerId r1) (runnerId r2)

newtype Description = Description Text deriving newtype (Eq)

newtype Tag = Tag Text deriving newtype (Eq)

data Job = Job
  { jobId :: Id Job,
    jobProjectId :: Id Project,
    jobProjectName :: Name Project,
    jobStage :: Stage,
    jobName :: Name Job,
    jobRef :: Ref,
    jobWebUrl :: Url Job
  }

newtype Stage = Stage Text

newtype IpAddress = IpAddress Text deriving newtype (Eq)

data RunnersJobs = NoSuccessfulUpdateYet | RunnersJobs (UTCTime, Map Runner [Job])

data RunnersApi m a where
  GetOnlineRunnersForGroup :: Id Group -> RunnersApi m (Either UpdateError [Runner])
  GetProjectRunnersForGroup :: Id Group -> RunnersApi m (Either UpdateError [(Id Project, [Runner])])
  GetRunningJobsForRunner :: Id Runner -> RunnersApi m (Either UpdateError [Job])

makeSem ''RunnersApi

data RunnersJobsApi m a where
  GetJobs :: RunnersJobsApi m RunnersJobs
  SetJobs :: Map Runner [Job] -> RunnersJobsApi m ()

makeSem ''RunnersJobsApi
