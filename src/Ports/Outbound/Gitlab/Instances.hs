{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ports.Outbound.Gitlab.Instances (buildStatusToApiString) where

import Core.BuildStatuses
import Core.Shared
import Data.Aeson hiding (Result, Value)
import Data.Aeson.Casing (aesonDrop, aesonPrefix, snakeCase)
import Network.URI (parseURI)
import Relude

instance FromJSON DetailedPipeline where
  parseJSON = withObject "detailedPipeline" $ \dp -> do
    detailedPipelineId <- dp .: "id"
    detailedPipelineRef <- dp .: "ref"
    detailedPipelineWebUrl <- dp .: "web_url"
    detailedPipelineStatus <- dp .: "detailed_status" >>= \ds -> ds .: "group"
    pure DetailedPipeline {..}

instance FromJSON BuildStatus where
  parseJSON = withText "BuildStatus" $ \x -> maybe (fail $ mconcat ["couldn't parse build status from '", show x, "'"]) pure (inverseMap buildStatusToApiString x)

buildStatusToApiString :: (IsString p) => BuildStatus -> p
buildStatusToApiString Unknown = "unknown"
buildStatusToApiString Cancelled = "canceled"
buildStatusToApiString Created = "created"
buildStatusToApiString Failed = "failed"
buildStatusToApiString Manual = "manual"
buildStatusToApiString Pending = "pending"
buildStatusToApiString Preparing = "preparing"
buildStatusToApiString Running = "running"
buildStatusToApiString Scheduled = "scheduled"
buildStatusToApiString Skipped = "skipped"
buildStatusToApiString Successful = "success"
buildStatusToApiString SuccessfulWithWarnings = "success-with-warnings"
buildStatusToApiString WaitingForResource = "waiting_for_resource"

instance FromJSON Pipeline where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON PipelineSource where
  parseJSON = withText "PipelineSource" $ \x -> maybe (fail $ mconcat ["couldn't parse pipeline source from '", show x, "'"]) pure (inverseMap pipelineSourceToApiString x)

pipelineSourceToApiString :: (IsString p) => PipelineSource -> p
pipelineSourceToApiString PipelineSourceApi = "api"
pipelineSourceToApiString PipelineSourceChat = "chat"
pipelineSourceToApiString PipelineSourceExternal = "external"
pipelineSourceToApiString PipelineSourceExternalPullRequestEvent = "external_pull_request_event"
pipelineSourceToApiString PipelineSourceMergeRequestEvent = "merge_request_event"
pipelineSourceToApiString PipelineSourceOnDemandDastScan = "ondemand_dast_scan"
pipelineSourceToApiString PipelineSourceOnDemandDastValidation = "ondemand_dast_validation"
pipelineSourceToApiString PipelineSourceParentPipeline = "parent_pipeline"
pipelineSourceToApiString PipelineSourcePipeline = "pipeline"
pipelineSourceToApiString PipelineSourcePush = "push"
pipelineSourceToApiString PipelineSourceSchedule = "schedule"
pipelineSourceToApiString PipelineSourceSecurityOrchestrationPolicy = "security_orchestration_policy"
pipelineSourceToApiString PipelineSourceTrigger = "trigger"
pipelineSourceToApiString PipelineSourceWeb = "web"
pipelineSourceToApiString PipelineSourceWebIDE = "webide"

instance FromJSON Schedule where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON DetailedSchedule where
  parseJSON = genericParseJSON $ aesonDrop (length @[] "DetailedSchedule") snakeCase

instance FromJSON (Url a) where
  parseJSON = withText "URI" $ \v -> Url <$> maybe (fail "Bad URI") pure (parseURI (toString v))

instance FromJSON Project where
  parseJSON = withObject "project" $ \project -> do
    projectId <- project .: "id"
    projectName <- project .: "name"
    projectWebUrl <- project .: "web_url"
    projectDefaultBranch <- project .:? "default_branch"
    projectNamespace <- project .: "namespace"
    pure Project {..}

instance FromJSON ProjectNamespace where
  parseJSON = withObject "namespace" $ \namespace -> do
    projectNamespaceId <- namespace .: "id"
    projectNamespaceFullPath <- namespace .: "full_path"
    pure ProjectNamespace {..}

deriving newtype instance FromJSON (Id a)

deriving newtype instance FromJSON (Name a)

deriving newtype instance FromJSON Ref

deriving newtype instance FromJSON ScheduleDescription

deriving newtype instance FromJSON ProjectNamespaceFullPath
