{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ports.Outbound.Gitlab.Instances (buildStatusToApiString) where

import Core.BuildStatuses
import Core.Shared
import Data.Aeson hiding (Result, Value)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
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

instance FromJSON (Url a) where
  parseJSON = withText "URI" $ \v -> Url <$> maybe (fail "Bad URI") pure (parseURI (toString v))

instance FromJSON Project where
  parseJSON = withObject "project" $ \project -> do
    projectId <- project .: "id"
    projectName <- project .: "name"
    projectWebUrl <- project .: "web_url"
    projectDefaultBranch <- project .: "default_branch"
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

deriving newtype instance FromJSON ProjectNamespaceFullPath
