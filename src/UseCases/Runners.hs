{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UseCases.Runners (updateRunnersJobs) where

import Core.BuildStatuses (Project)
import Core.Effects
import Core.Runners
import Core.Shared
import Data.Aeson (ToJSON)
import Data.Map (fromAscListWith, mapKeys)
import Polysemy
import qualified Polysemy.Reader as R
import Relude

updateRunnersJobs :: (Member RunnersApi r, Member RunnersJobsApi r, Member Logger r, Member ParTraverse r, Member (R.Reader (Id Group)) r, Member (R.Reader [Id Project]) r) => Sem r (Map Runner [Job])
updateRunnersJobs = do
  currentJobs <- currentKnownRunnersJobs
  setJobs currentJobs
  logCurrentRunnersJobs
  pure currentJobs

currentKnownRunnersJobs :: (Member RunnersApi r, Member Logger r, Member ParTraverse r, Member (R.Reader (Id Group)) r, Member (R.Reader [Id Project]) r) => Sem r (Map Runner [Job])
currentKnownRunnersJobs = do
  runner <- findRunners
  results <- traverseP evalRunner runner
  pure $ fromAscListWith (<>) $ catMaybes results

findRunners :: (Member RunnersApi r, Member Logger r, Member (R.Reader (Id Group)) r) => Sem r [Runner]
findRunners = do
  groupId <- R.ask
  addContext "groupId" groupId $ do
    result <- getOnlineRunnersForGroup groupId
    case result of
      Left err -> [] <$ logWarn (unwords ["Couldn't load runners. Error was", show err])
      Right runners -> pure runners

logCurrentRunnersJobs :: (Member RunnersJobsApi r, Member Logger r) => Sem r ()
logCurrentRunnersJobs = do
  result <- getJobs
  case result of
    NoSuccessfulUpdateYet -> logDebug "There was no successful update yet, so there are no runners jobs available"
    (RunnersJobs (_, jobs)) -> do
      if null jobs
        then logDebug "No running jobs found"
        else addContext "jobs" (fmap jobId <$> mapKeys (show @Text . runnerId) jobs) $ logDebug "Running jobs found"

evalRunner :: (Member RunnersApi r, Member Logger r, Member (R.Reader (Id Group)) r, Member (R.Reader [Id Project]) r) => Runner -> Sem r (Maybe (Runner, [Job]))
evalRunner r@Runner {..} = addContext "runnerId" runnerId $ do
  groupId <- R.ask
  excludeList <- R.ask
  jobs <- getRunningJobsForRunner groupId runnerId
  case jobs of
    Left err -> Nothing <$ logWarn (unwords ["Couldn't get jobs for runner. Error was", show err])
    Right js -> pure $ Just (r, filter (\j -> jobProjectId j `notElem` excludeList) js)

deriving newtype instance ToJSON (Id a)
