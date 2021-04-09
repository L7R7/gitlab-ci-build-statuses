{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Core.UseCases.Lib
  ( updateStatuses,
    currentBuildStatuses,
    getStatusForProject,
  )
where

import Core.Effects (Logger, ParTraverse, addContext, logDebug, logWarn, traverseP)
import Core.Lib
import Data.List (partition)
import qualified Data.Text as T (intercalate, toLower)
import Polysemy
import Relude

updateStatuses :: (Member ProjectsApi r, Member PipelinesApi r, Member BuildStatusesApi r, Member Logger r, Member ParTraverse r) => Id Group -> Sem r [Result]
updateStatuses groupId = do
  currentStatuses <- currentKnownBuildStatuses groupId
  unless (null currentStatuses) $ setStatuses currentStatuses
  logCurrentBuildStatuses
  pure currentStatuses

currentKnownBuildStatuses :: (Member ProjectsApi r, Member PipelinesApi r, Member Logger r, Member ParTraverse r) => Id Group -> Sem r [Result]
currentKnownBuildStatuses groupId = filter ((/= Unknown) . buildStatus) <$> currentBuildStatuses groupId

currentBuildStatuses :: (Member ParTraverse r, Member ProjectsApi r, Member PipelinesApi r, Member Logger r) => Id Group -> Sem r [Result]
currentBuildStatuses groupId = do
  projects <- findProjects groupId
  results <- traverseP evalProject projects
  pure $ sortOn (T.toLower . coerce . name) results

findProjects :: (Member ProjectsApi r, Member Logger r) => Id Group -> Sem r [Project]
findProjects groupId = addContext "groupId" groupId $ do
  result <- getProjects groupId
  case result of
    Left err -> [] <$ logWarn (unwords ["Couldn't load projects. Error was", show err])
    Right ps -> pure ps

logCurrentBuildStatuses :: (Member BuildStatusesApi r, Member Logger r) => Sem r ()
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

evalProject :: (Member PipelinesApi r, Member Logger r) => Project -> Sem r Result
evalProject p@Project {..} = toResult p <$> getStatusForProject projectId projectDefaultBranch

getStatusForProject :: (Member PipelinesApi r, Member Logger r) => Id Project -> Maybe Ref -> Sem r (Maybe (BuildStatus, Url Pipeline))
getStatusForProject _ Nothing = pure Nothing
getStatusForProject projectId (Just defaultBranch) = addContext "projectId" projectId $ do
  pipeline <- getLatestPipelineForRef projectId defaultBranch
  case pipeline of
    Left EmptyPipelinesResult -> pure Nothing
    Left NoPipelineForDefaultBranch -> pure Nothing
    Left uError -> Nothing <$ logWarn (unwords ["Couldn't eval project. Error was", show uError])
    Right p -> do
      let st = pipelineStatus p
      detailedStatus <- if st == Successful then detailedStatusForPipeline projectId (pipelineId p) else pure Nothing
      pure $ Just (fromMaybe st detailedStatus, pipelineWebUrl p)

detailedStatusForPipeline :: (Member PipelinesApi r, Member Logger r) => Id Project -> Id Pipeline -> Sem r (Maybe BuildStatus)
detailedStatusForPipeline projectId pipelineId =
  addContext "pipelineId" pipelineId $ do
    singlePipelineResult <- getSinglePipeline projectId pipelineId
    case singlePipelineResult of
      Left uError -> Nothing <$ logWarn (unwords ["Couldn't get details for pipeline, error was", show uError])
      Right dp -> pure . Just $ detailedPipelineStatus dp
