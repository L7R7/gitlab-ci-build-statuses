{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Core.Jobs
  ( Job (..),
    JobsApi (..),
    getJobsWithStatuses,
    WaitingJobs (..),
    WaitingJobsApi (..),
    getJobs,
    setJobs,
  )
where

import Core.BuildStatuses (BuildStatus, Project)
import Core.Shared
import Data.Time (UTCTime)
import Polysemy (makeSem)
import Relude

data Job = Job
  { jobId :: Id Job,
    jobStatus :: BuildStatus
  }

data JobsApi m a where
  GetJobsWithStatuses :: Id Project -> NonEmpty BuildStatus -> JobsApi m (Either UpdateError [Job])

makeSem ''JobsApi

data WaitingJobs = NoSuccessfulUpdateYet | WaitingJobs (UTCTime, Map BuildStatus [Job])

data WaitingJobsApi m a where
  GetJobs :: WaitingJobsApi m WaitingJobs
  SetJobs :: Map BuildStatus [Job] -> WaitingJobsApi m ()

makeSem ''WaitingJobsApi
