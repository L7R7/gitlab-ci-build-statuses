{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Core.Jobs
  ( Job (..),
    JobsApi (..),
    getJobsWithStatuses,
  )
where

import Core.BuildStatuses (BuildStatus, Project)
import Core.Shared
import Polysemy (makeSem)
import Relude

data Job = Job
  { jobId :: Id Job,
    jobStatus :: BuildStatus
  }

data JobsApi m a where
  GetJobsWithStatuses :: Id Project -> NonEmpty BuildStatus -> JobsApi m (Either UpdateError [Job])

makeSem ''JobsApi
