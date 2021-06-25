{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Core.Runners
  ( Runner (..),
    Job (..),
    Stage (..),
    IpAddress (..),
    RunnersJobs (..),
    RunnersApi (..),
    getOnlineRunnersForGroup,
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
    runnerName :: Name Runner,
    runnerIpAddress :: IpAddress
  }
  deriving stock (Eq, Generic)

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
  GetRunningJobsForRunner :: Id Group -> Id Runner -> RunnersApi m (Either UpdateError [Job])

makeSem ''RunnersApi

data RunnersJobsApi m a where
  GetJobs :: RunnersJobsApi m RunnersJobs
  SetJobs :: Map Runner [Job] -> RunnersJobsApi m ()

makeSem ''RunnersJobsApi
