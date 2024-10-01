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

import Core.Shared
import Gitlab.Job (JobStatus)
import Gitlab.Lib (Id)
import Gitlab.Project (Project)
import Polysemy (makeSem)
import Relude

data Job = Job
  { jobId :: Id Job,
    jobStatus :: JobStatus
  }

data JobsApi m a where
  GetJobsWithStatuses :: Id Project -> NonEmpty JobStatus -> JobsApi m (Either UpdateError [Job])

makeSem ''JobsApi
