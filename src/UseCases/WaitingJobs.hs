{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UseCases.WaitingJobs
  ( updateWaitingJobs,
  )
where

import Core.BuildStatuses hiding (BuildStatuses (..))
import Core.Effects (Logger, ParTraverse, addContext, logDebug, logWarn, traverseP)
import Core.Jobs
import Core.Shared
import qualified Data.Map as M
import Polysemy
import qualified Polysemy.Reader as R
import Relude
import UseCases.Shared (findProjects)

updateWaitingJobs :: (Member ProjectsApi r, Member JobsApi r, Member WaitingJobsApi r, Member Logger r, Member ParTraverse r, Member (R.Reader (Id Group)) r, Member (R.Reader [Id Project]) r) => Sem r (Map BuildStatus [Job])
updateWaitingJobs = do
  waitingJobs <- currentWaitingJobs
  setJobs waitingJobs
  logCurrentWaitingJobs
  pure waitingJobs

currentWaitingJobs :: (Member ProjectsApi r, Member JobsApi r, Member ParTraverse r, Member Logger r, Member (R.Reader (Id Group)) r, Member (R.Reader [Id Project]) r) => Sem r (Map BuildStatus [Job])
currentWaitingJobs = do
  projects <- findProjects
  results <- traverseP evalProject projects
  pure $ groupByStatus (join results)

logCurrentWaitingJobs :: (Member WaitingJobsApi r, Member Logger r) => Sem r ()
logCurrentWaitingJobs = do
  result <- getJobs
  case result of
    NoSuccessfulUpdateYet -> logDebug "There was no successful update yet, so there are no waiting jobs available"
    (WaitingJobs (_, jobs)) -> logDebug $ "waiting jobs " <> show (fmap jobId <$> jobs)

evalProject :: (Member JobsApi r, Member Logger r) => Project -> Sem r [Job]
evalProject Project {..} = getWaitingJobsForProject projectId

getWaitingJobsForProject :: (Member JobsApi r, Member Logger r) => Id Project -> Sem r [Job]
getWaitingJobsForProject projectId = addContext "projectId" projectId $ do
  let waitingStatuses = Created :| [Pending, Preparing, Scheduled, WaitingForResource]
  result <- getJobsWithStatuses projectId waitingStatuses
  case result of
    Left uError -> [] <$ logWarn (unwords ["Couldn't eval project. Error was", show uError])
    Right jobs -> pure jobs

groupByStatus :: [Job] -> Map BuildStatus [Job]
groupByStatus jobs = M.fromListWith (<>) ((\job -> (jobStatus job, [job])) <$> jobs)
