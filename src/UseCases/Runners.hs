{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UseCases.Runners (updateRunnersJobs) where

import Core.BuildStatuses (Project)
import Core.Effects
import Core.Runners
import Core.Shared
import Data.List.Extra (nubOrdOn)
import Data.Map (fromAscListWith, mapKeys)
import Polysemy
import Polysemy.Reader qualified as R
import Relude
import Relude.Extra (traverseToSnd)
import UseCases.Shared ()

updateRunnersJobs ::
  ( Member RunnersApi r,
    Member RunnersJobsApi r,
    Member Logger r,
    Member ParTraverse r,
    Member (R.Reader (NonEmpty (Id Group))) r,
    Member (R.Reader [Id Project]) r
  ) =>
  Sem r (Map Runner [Job])
updateRunnersJobs = do
  currentJobs <- currentKnownRunnersJobs
  setJobs currentJobs
  logCurrentRunnersJobs
  pure currentJobs

currentKnownRunnersJobs ::
  ( Member RunnersApi r,
    Member Logger r,
    Member ParTraverse r,
    Member (R.Reader (NonEmpty (Id Group))) r,
    Member (R.Reader [Id Project]) r
  ) =>
  Sem r (Map Runner [Job])
currentKnownRunnersJobs = do
  runnersWithGroups <- findRunners
  let runners = ordNub $ toList runnersWithGroups >>= toList . snd
  results <- catMaybes <$> traverseP evalRunner runners
  pure $ fromAscListWith (<>) results

findRunners ::
  ( Member RunnersApi r,
    Member Logger r,
    Member (R.Reader (NonEmpty (Id Group))) r
  ) =>
  Sem r (NonEmpty (Id Group, [Runner]))
findRunners = do
  groups <- R.ask
  traverse (traverseToSnd findRunnersForGroup) groups

findRunnersForGroup ::
  ( Member RunnersApi r,
    Member Logger r
  ) =>
  Id Group ->
  Sem r [Runner]
findRunnersForGroup groupId =
  addContext "groupId" groupId $ do
    result <- getOnlineRunnersForGroup groupId
    case result of
      Left err -> [] <$ logWarn (unwords ["Couldn't load group runners. Error was", show err])
      Right groupRunners -> do
        projectResult <- getProjectRunnersForGroup groupId
        projectRunners <- case projectResult of
          Left err -> [] <$ logWarn (unwords ["Couldn't load project runners. Error was", show err])
          Right runners -> pure $ runners >>= snd
        pure $ nubOrdOn runnerId groupRunners <> projectRunners

logCurrentRunnersJobs ::
  ( Member RunnersJobsApi r,
    Member Logger r
  ) =>
  Sem r ()
logCurrentRunnersJobs = do
  result <- getJobs
  case result of
    NoSuccessfulUpdateYet -> logDebug "There was no successful update yet, so there are no runners jobs available"
    (RunnersJobs (_, jobs)) -> do
      if null jobs
        then logDebug "No running jobs found"
        else addContext "jobs" (fmap jobId <$> mapKeys (show @Text . runnerId) jobs) $ logDebug "Running jobs found"

evalRunner ::
  ( Member RunnersApi r,
    Member Logger r,
    Member (R.Reader [Id Project]) r
  ) =>
  Runner ->
  Sem r (Maybe (Runner, [Job]))
evalRunner r@Runner {..} = addContext "runnerId" runnerId $ do
  excludeList <- R.ask
  jobs <- getRunningJobsForRunner runnerId
  case jobs of
    Left err -> Nothing <$ logWarn (unwords ["Couldn't get jobs for runner. Error was", show err])
    Right js -> pure $ Just (r, filter (\j -> jobProjectId j `notElem` excludeList) js)
