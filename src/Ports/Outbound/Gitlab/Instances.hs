{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ports.Outbound.Gitlab.Instances (jobStatusToApiString) where

import Core.BuildStatuses
import Data.Aeson hiding (Result, Value)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Gitlab.Job (JobStatus (..))
import Gitlab.Lib (Id (..), Name (..), Ref (..), Url (..))
import Gitlab.Project (ProjectNamespaceFullPath (..))
import Network.URI (parseURI)
import Relude

instance FromJSON DetailedPipeline where
  parseJSON = withObject "detailedPipeline" $ \dp -> do
    detailedPipelineId <- dp .: "id"
    detailedPipelineRef <- dp .: "ref"
    detailedPipelineWebUrl <- dp .: "web_url"
    detailedPipelineStatus <- dp .: "detailed_status" >>= \ds -> ds .: "group"
    pure DetailedPipeline {..}

jobStatusToApiString :: (IsString p) => JobStatus -> p
jobStatusToApiString Unknown = "unknown"
jobStatusToApiString Cancelled = "canceled"
jobStatusToApiString Created = "created"
jobStatusToApiString Failed = "failed"
jobStatusToApiString Manual = "manual"
jobStatusToApiString Pending = "pending"
jobStatusToApiString Preparing = "preparing"
jobStatusToApiString Running = "running"
jobStatusToApiString Scheduled = "scheduled"
jobStatusToApiString Skipped = "skipped"
jobStatusToApiString Successful = "success"
jobStatusToApiString SuccessfulWithWarnings = "success-with-warnings"
jobStatusToApiString WaitingForResource = "waiting_for_resource"

instance FromJSON Pipeline where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON (Url a) where
  parseJSON = withText "URI" $ \v -> Url <$> maybe (fail "Bad URI") pure (parseURI (toString v))

-- todo derive via codec
deriving newtype instance FromJSON (Id a)

-- todo derive via codec
deriving newtype instance FromJSON (Name a)

-- todo derive via codec
deriving newtype instance FromJSON Ref

-- todo derive via codec
deriving newtype instance FromJSON ProjectNamespaceFullPath
