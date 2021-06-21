{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Core.Runners where

import Core.Lib (Group, Id, Project, Ref, UpdateError, Url, ProjectName (ProjectName))
import Data.Time (UTCTime)
import Polysemy (makeSem)
import Relude

-- data UpdateError
--   = HttpError HttpException
--   | ConversionError JSONException
--   deriving stock (Show)

data Runner = Runner
  { runnerId :: Id Runner,
    runnerName :: RunnerName,
    runnerIpAddress :: IpAddress
  }
  deriving stock (Eq, Generic)
  deriving (Show)

data Job = Job
  { jobId :: Id Job,
    jobProjectId :: Id Project,
    jobProjectName :: ProjectName,
    jobStage :: Stage,
    jobName :: JobName,
    jobRef :: Ref,
    jobWebUrl :: Url Job
  } deriving Show

newtype Stage = Stage Text deriving Show

newtype IpAddress = IpAddress Text deriving newtype (Eq) deriving Show

newtype JobName = JobName Text deriving Show

newtype RunnerName = RunnerName Text deriving newtype (Eq)
  deriving (Show)

data RunnersJobs = NoSuccessfulUpdateYet | RunnersJobs (UTCTime, Map Runner [Job]) deriving (Show)

-- todo: decouple update error
data RunnersApi m a where
  GetOnlineRunnersForGroup :: Id Group -> RunnersApi m (Either UpdateError [Runner])
  GetRunningJobsForRunner :: Id Group -> Id Runner -> RunnersApi m (Either UpdateError [Job])

makeSem ''RunnersApi

data RunnersJobsApi m a where
  GetJobs :: RunnersJobsApi m RunnersJobs
  SetJobs :: Map Runner [Job] -> RunnersJobsApi m ()

makeSem ''RunnersJobsApi
