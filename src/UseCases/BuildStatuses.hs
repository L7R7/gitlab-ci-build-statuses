{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UseCases.BuildStatuses
  ( updateStatuses,
    currentBuildStatuses,
    getStatusForProject,
  )
where

import Core.BuildStatuses
import Core.Effects (Logger, ParTraverse, addContext, logDebug, logWarn, traverseP)
import Core.Shared
import Data.List (partition)
import Data.Text qualified as T (intercalate, toLower)
import Polysemy
import Polysemy.Reader qualified as R
import Relude
import UseCases.Shared ()

updateStatuses ::
  ( Member ProjectsWithoutExcludesApi r,
    Member PipelinesApi r,
    Member BuildStatusesApi r,
    Member Logger r,
    Member ParTraverse r,
    Member (R.Reader (Id Group)) r
  ) =>
  Sem r [Result]
updateStatuses = do
  currentStatuses <- currentKnownBuildStatuses
  unless (null currentStatuses) $ setStatuses currentStatuses
  logCurrentBuildStatuses
  pure currentStatuses

currentKnownBuildStatuses ::
  ( Member ProjectsWithoutExcludesApi r,
    Member PipelinesApi r,
    Member Logger r,
    Member ParTraverse r,
    Member (R.Reader (Id Group)) r
  ) =>
  Sem r [Result]
currentKnownBuildStatuses = filter ((/= Unknown) . buildStatus) <$> currentBuildStatuses

currentBuildStatuses ::
  ( Member ParTraverse r,
    Member ProjectsWithoutExcludesApi r,
    Member PipelinesApi r,
    Member Logger r,
    Member (R.Reader (Id Group)) r
  ) =>
  Sem r [Result]
currentBuildStatuses = do
  groupId <- R.ask
  projects <- getProjectsNotOnExcludeListOrEmpty groupId
  results <- traverseP evalProject projects
  pure $ sortOn (T.toLower . coerce . name) results

logCurrentBuildStatuses ::
  ( Member BuildStatusesApi r,
    Member Logger r
  ) =>
  Sem r ()
logCurrentBuildStatuses = do
  result <- getStatuses
  case result of
    NoSuccessfulUpdateYet -> logDebug "There was no successful update yet, so there are no pipeline statuses available"
    (Statuses (_, statuses)) -> do
      let (unknown, known) = partition ((== Unknown) . buildStatus) statuses
      if null known
        then logWarn "No valid Pipeline statuses found"
        else addContext "projectIds" (concatIds known) $ logDebug "Pipeline statuses found"
      unless (null unknown) (logDebug $ "No pipelines found for projects " <> concatIds unknown)
  where
    concatIds :: [Result] -> Text
    concatIds rs = T.intercalate ", " (show . projId <$> rs)

evalProject ::
  ( Member PipelinesApi r,
    Member Logger r
  ) =>
  Project ->
  Sem r Result
evalProject p@Project {..} = toResult p <$> getStatusForProject projectId projectDefaultBranch

getStatusForProject ::
  ( Member PipelinesApi r,
    Member Logger r
  ) =>
  Id Project ->
  Maybe Ref ->
  Sem r (Maybe (BuildStatus, Url Pipeline))
getStatusForProject _ Nothing = pure Nothing
getStatusForProject projectId (Just defaultBranch) = addContext "projectId" projectId $ do
  pipeline <- getLatestPipelineForRef projectId defaultBranch
  case pipeline of
    Left EmptyResult -> pure Nothing
    Left uError -> Nothing <$ logWarn (unwords ["Couldn't eval project. Error was", show uError])
    Right p -> do
      let st = pipelineStatus p
      detailedStatus <- if st == Successful then detailedStatusForPipeline projectId (pipelineId p) else pure Nothing
      pure $ Just (fromMaybe st detailedStatus, pipelineWebUrl p)

detailedStatusForPipeline ::
  ( Member PipelinesApi r,
    Member Logger r
  ) =>
  Id Project ->
  Id Pipeline ->
  Sem r (Maybe BuildStatus)
detailedStatusForPipeline projectId pipelineId =
  addContext "pipelineId" pipelineId $ do
    singlePipelineResult <- getSinglePipeline projectId pipelineId
    case singlePipelineResult of
      Left uError -> Nothing <$ logWarn (unwords ["Couldn't get details for pipeline, error was", show uError])
      Right dp -> pure . Just $ detailedPipelineStatus dp
