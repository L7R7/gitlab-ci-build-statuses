{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Core.UseCases.Runners
  ( updateRunnersJobs,
    currentKnownRunnersJobs,
  )
where

import Core.Effects (Logger, ParTraverse, addContext, logDebug, logWarn, traverseP)
import Core.Lib hiding (NoSuccessfulUpdateYet)
import Core.Runners
import Data.Map (fromAscListWith, mapKeys)
import Polysemy
import Relude

updateRunnersJobs :: (Member RunnersApi r, Member RunnersJobsApi r, Member Logger r, Member ParTraverse r) => Id Group -> [Id Project] -> Sem r (Map Runner [Job])
updateRunnersJobs groupId excludeList = do
  currentStatuses <- currentKnownRunnersJobs groupId excludeList
  setJobs currentStatuses
  logCurrentRunnersJobs
  pure currentStatuses

currentKnownRunnersJobs :: (Member RunnersApi r, Member Logger r, Member ParTraverse r) => Id Group -> [Id Project] -> Sem r (Map Runner [Job])
currentKnownRunnersJobs groupId excludeList = do
  runner <- findRunners groupId
  results <- traverseP (evalRunner groupId excludeList) runner
  pure $ fromAscListWith (<>) $ catMaybes results

findRunners :: (Member RunnersApi r, Member Logger r) => Id Group -> Sem r [Runner]
findRunners groupId = addContext "groupId" groupId $ do
  result <- getOnlineRunnersForGroup groupId
  case result of
    Left err -> [] <$ logWarn (unwords ["Couldn't load runners. Error was", show err])
    Right runners -> pure runners

logCurrentRunnersJobs :: (Member RunnersJobsApi r, Member Logger r) => Sem r ()
logCurrentRunnersJobs = do
  result <- getJobs
  case result of
    NoSuccessfulUpdateYet -> logDebug "There was no successful update yet, so there are no pipeline statuses available"
    (RunnersJobs (_, jobs)) -> do
      if null jobs
        then logDebug "No running jobs found"
        else addContext "jobs" (fmap jobId <$> mapKeys (show @Text . runnerId) jobs) $ logDebug "Running jobs found"

evalRunner :: (Member RunnersApi r, Member Logger r) => Id Group -> [Id Project] -> Runner -> Sem r (Maybe (Runner, [Job]))
evalRunner groupId excludeList r@Runner {..} = addContext "runnerId" runnerId $ do
  jobs <- getRunningJobsForRunner groupId runnerId
  case jobs of
    Left err -> Nothing <$ logWarn (unwords ["Couldn't get jobs for runner. Error was", show err])
    Right js -> pure $ Just (r, filter (\j -> jobProjectId j `notElem` excludeList) js)
