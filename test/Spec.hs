{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

import Core.Effects
import Core.Lib
import Network.URI.Static
import Polysemy
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "currentBuildStatuses" $ do
  it "returns an empty list when there are no projects" $ (run . evaluateEffectsPurely $ currentBuildStatuses (Id 266)) `shouldBe` []
  it "returns the correct result for a single project" $ (run . evaluateEffectsPurely $ currentBuildStatuses (Id 42)) `shouldBe` [result]
  where
    evaluateEffectsPurely = noOpLogger . pipelinesApiPure . projectsApiPure . parTraversePure
    result = Result (Id 512) (ProjectName "myProj") Unknown (Left (Url $$(staticURI "https://my.gitlab.com/projects/42/foo")))

projectsApiPure :: InterpreterFor ProjectsApi r
projectsApiPure = interpret $ \case
  GetProjects (Id 42) -> pure (Right [project])
  GetProjects _ -> pure (Left EmptyPipelinesResult)

project :: Project
project = Project (Id 512) (ProjectName "myProj") (Url $$(staticURI "https://my.gitlab.com/projects/42/foo")) Nothing

pipelinesApiPure :: InterpreterFor PipelinesApi r
pipelinesApiPure = interpret $ \case
  GetLatestPipelineForRef _ _ -> pure $ Left NoPipelineForDefaultBranch
  GetSinglePipeline _ _ -> pure $ Left NoPipelineForDefaultBranch

parTraversePure :: InterpreterFor ParTraverse r
parTraversePure =
  interpretH $ \case
    TraverseP f ta -> raise . parTraversePure =<< runT (traverse f ta)

noOpLogger :: Sem (Logger ': r) a -> Sem r a
noOpLogger = do
  interpretH $ \case
    LogInfo _ -> pureT ()
    LogWarn _ -> pureT ()
    LogError _ -> pureT ()
    AddContext _ _ action -> raise . noOpLogger =<< runT action
    AddContexts _ action -> raise . noOpLogger =<< runT action
    AddNamespace _ action -> raise . noOpLogger =<< runT action
