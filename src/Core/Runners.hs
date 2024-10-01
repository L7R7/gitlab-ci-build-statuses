{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Core.Runners
  ( Job (..),
    Stage (..),
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

import Core.Shared (UpdateError)
import Data.Time (UTCTime)
import Gitlab.Group (Group)
import Gitlab.Lib (Id, Name, Ref, Url)
import Gitlab.Project (Project)
import Gitlab.Runner
import Polysemy (makeSem)
import Relude

instance Ord Runner where
  compare r1 r2 = compare (runnerId r1) (runnerId r2)

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
