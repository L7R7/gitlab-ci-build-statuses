{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module TestUtils where

import Core.Effects
import Core.Lib
import qualified Data.Map as M
import Polysemy
import Relude

projectsApiFromMap :: Map (Id Group) [Project] -> InterpreterFor ProjectsApi r
projectsApiFromMap inputs = interpret $ \case
  GetProjects pId -> pure $ maybeToRight EmptyResult (M.lookup pId inputs)

pipelinesApiFromMaps :: Map (Id Project, Ref) Pipeline -> Map (Id Project, Id Pipeline) DetailedPipeline -> InterpreterFor PipelinesApi r
pipelinesApiFromMaps pipelines detailedPipelines = interpret $ \case
  GetLatestPipelineForRef pId ref -> pure $ maybeToRight EmptyResult (M.lookup (pId, ref) pipelines)
  GetSinglePipeline pId pipelId -> pure $ maybeToRight EmptyResult (M.lookup (pId, pipelId) detailedPipelines)

parTraversePure :: InterpreterFor ParTraverse r
parTraversePure =
  interpretH $ \case
    TraverseP f ta -> raise . parTraversePure =<< runT (traverse f ta)

noOpLogger :: InterpreterFor Logger r
noOpLogger = do
  interpretH $ \case
    LogDebug _ -> pureT ()
    LogInfo _ -> pureT ()
    LogWarn _ -> pureT ()
    LogError _ -> pureT ()
    AddContext _ _ action -> raise . noOpLogger =<< runT action
    AddContexts _ action -> raise . noOpLogger =<< runT action
    AddNamespace _ action -> raise . noOpLogger =<< runT action
