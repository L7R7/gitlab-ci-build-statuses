{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UseCases.BuildStatuses
  ( updateStatuses,
    currentBuildStatuses,
    getStatusForProject,
  )
where

import Config.Config (ExtraProjectsList (ExtraProjectsList))
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
  ( Member ProjectsApi r,
    Member ProjectsWithoutExcludesApi r,
    Member PipelinesApi r,
    Member SchedulesApi r,
    Member BuildStatusesApi r,
    Member Logger r,
    Member ParTraverse r,
    Member (R.Reader ExtraProjectsList) r,
    Member (R.Reader [Id Group]) r
  ) =>
  Sem r [Result]
updateStatuses = do
  currentStatuses <- currentKnownBuildStatuses
  unless (null currentStatuses) $ setStatuses currentStatuses
  logCurrentBuildStatuses
  pure currentStatuses

currentKnownBuildStatuses ::
  ( Member ProjectsApi r,
    Member ProjectsWithoutExcludesApi r,
    Member PipelinesApi r,
    Member SchedulesApi r,
    Member Logger r,
    Member ParTraverse r,
    Member (R.Reader ExtraProjectsList) r,
    Member (R.Reader [Id Group]) r
  ) =>
  Sem r [Result]
currentKnownBuildStatuses = filter ((/= Unknown) . buildStatus) <$> currentBuildStatuses

currentBuildStatuses ::
  ( Member ProjectsApi r,
    Member ParTraverse r,
    Member ProjectsWithoutExcludesApi r,
    Member PipelinesApi r,
    Member SchedulesApi r,
    Member Logger r,
    Member (R.Reader ExtraProjectsList) r,
    Member (R.Reader [Id Group]) r
  ) =>
  Sem r [Result]
currentBuildStatuses = do
  projects <- findProjects
  results <- traverseP evalProject projects
  pure $ sortOn (T.toLower . coerce . name) (join results)

findProjects ::
  ( Member ProjectsApi r,
    Member ProjectsWithoutExcludesApi r,
    Member ParTraverse r,
    Member Logger r,
    Member (R.Reader ExtraProjectsList) r,
    Member (R.Reader [Id Group]) r
  ) =>
  Sem r [Project]
findProjects = do
  groupIds <- R.ask
  groupProjects <- join <$> traverseP getProjectsNotOnExcludeListOrEmpty (toList groupIds)
  (ExtraProjectsList extraProjectIds) <- R.ask
  extraProjectsResult <- sequence <$> traverseP getProject extraProjectIds
  extraProjects <- case extraProjectsResult of
    Left uErr -> [] <$ logWarn (unwords ["Couldn't fetch information for all extra projects. Skipping the extra projects for now. Error was", show uErr])
    Right ps -> pure ps
  pure $ ordNub $ groupProjects <> extraProjects

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
    Member SchedulesApi r,
    Member Logger r
  ) =>
  Project ->
  Sem r [Result]
evalProject p@Project {..} = do
  nonScheduled <- toResult p <$> getStatusForProject projectId projectDefaultBranch
  scheduledStatuses <- getStatusesForSchedules projectId
  pure $ nonScheduled : (toResult p . (\(bs, ps, sd, up) -> Just (bs, ps, getScheduleDescription sd, up)) <$> scheduledStatuses) -- todo: improve toResult, maybe get rid of the Unknown status entirely?

getStatusForProject ::
  ( Member PipelinesApi r,
    Member Logger r
  ) =>
  Id Project ->
  Maybe Ref ->
  Sem r (Maybe (BuildStatus, PipelineSource, Text, Url Pipeline))
getStatusForProject _ Nothing = pure Nothing
getStatusForProject projectId (Just defaultBranch) = addContext "projectId" projectId $ do
  pipeline <- getLatestPipelineForRef projectId defaultBranch
  case pipeline of
    Left EmptyResult -> pure Nothing
    Left uError -> Nothing <$ logWarn (unwords ["Couldn't eval project. Error was", show uError])
    Right p -> do
      let st = pipelineStatus p
      detailedStatus <- if st == Successful then detailedStatusForPipeline projectId (pipelineId p) else pure Nothing
      pure $ Just (fromMaybe st detailedStatus, pipelineSource p, getRef defaultBranch, pipelineWebUrl p)

getStatusesForSchedules ::
  ( Member PipelinesApi r,
    Member SchedulesApi r,
    Member Logger r
  ) =>
  Id Project ->
  Sem r [(BuildStatus, PipelineSource, ScheduleDescription, Url Pipeline)]
getStatusesForSchedules projectId = do
  schedulesResult <- getActiveSchedulesForProject projectId
  case schedulesResult of
    Left uError -> [] <$ logWarn (unwords ["Couldn't get schedules for project, error was", show uError])
    Right schedules -> do
      catMaybes <$> traverse (resultForSchedule projectId) schedules

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

resultForSchedule ::
  ( Member PipelinesApi r,
    Member SchedulesApi r,
    Member Logger r
  ) =>
  Id Project ->
  Schedule ->
  Sem r (Maybe (BuildStatus, PipelineSource, ScheduleDescription, Url Pipeline))
resultForSchedule projectId schedule = do
  scheduleResult <- getSchedule projectId (scheduleId schedule)
  case scheduleResult of
    Left uError -> Nothing <$ logWarn (unwords ["Couldn't get latest pipeline for schedule", show (scheduleId schedule), "error was", show uError])
    Right s -> case detailedScheduleLastPipeline s of
      Nothing -> pure Nothing
      Just p -> do
        let st = pipelineStatus p
        detailedStatus <- if st == Successful then detailedStatusForPipeline projectId (pipelineId p) else pure Nothing
        pure $ Just (fromMaybe st detailedStatus, pipelineSource p, scheduleDescription schedule, pipelineWebUrl p)
