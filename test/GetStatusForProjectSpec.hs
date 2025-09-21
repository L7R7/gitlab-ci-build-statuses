{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module GetStatusForProjectSpec where

import Config.Config (ExtraProjectsList (ExtraProjectsList), ProjectExcludeList (ProjectExcludeList))
import Core.BuildStatuses
import Core.Shared
import Data.Map qualified as M
import Network.URI.Static
import Path
import Polysemy
import Polysemy.Reader qualified as R
import Ports.Outbound.Gitlab.Projects (projectsWithoutExcludesApiInTermsOfProjects)
import Relude
import Test.Hspec
import TestUtils
import UseCases.BuildStatuses

spec :: Spec
spec = do
  describe "currentBuildStatuses" $ do
    it "returns an empty list when there are no projects" $ (run . R.runReader [Id 266] . R.runReader (ExtraProjectsList []) . R.runReader (ProjectExcludeList []) . evaluateEffectsPurely $ currentBuildStatuses) `shouldBe` []
    it "returns the correct result for a single project" $ (run . R.runReader [Id 42] . R.runReader (ExtraProjectsList []) . R.runReader (ProjectExcludeList []) . evaluateEffectsPurely $ currentBuildStatuses) `shouldBe` result

  describe "getStatusForProject" $ do
    it "returns Nothing when there's no default branch" $ (run . pipelinesApiPure . noOpLogger $ getStatusForProject (Id 5) Nothing) `shouldBe` Nothing
    it "returns the correct status for a project that doesn't require the detailed status"
      $ (\(bs, _, _, _) -> bs)
      <$> (run . pipelinesApiPure . noOpLogger $ getStatusForProject (Id 311) (Just $ Ref "main"))
      `shouldBe` Just Running
    it "returns the correct status for a project that passed with warnings"
      $ (\(bs, _, _, _) -> bs)
      <$> (run . pipelinesApiFromMaps projects pipelines . noOpLogger $ getStatusForProject (Id 50) (Just $ Ref "main"))
      `shouldBe` Just SuccessfulWithWarnings
    it "returns the correct status for a project that was successful but has no result for the detailed pipeline"
      $ (\(bs, _, _, _) -> bs)
      <$> (run . pipelinesApiFromMaps projects pipelines . noOpLogger $ getStatusForProject (Id 52) (Just $ Ref "main"))
      `shouldBe` Just Successful
  where
    evaluateEffectsPurely =
      noOpLogger
        . emptySchedulesApi
        . pipelinesApiPure
        . projectsApiFromMap
          ( M.fromList
              [ ( Id 42,
                  [ Project (Id 312) (Name "myProj") (Url $$(staticURI "https://my.gitlab.com/projects/512/foo")) Nothing (ProjectNamespace (Id 12) (ProjectNamespaceFullPath $(mkRelDir "foo"))),
                    Project (Id 311) (Name "my-other-project") (Url $$(staticURI "https://my.gitlab.com/projects/311/bar")) (Just $ Ref "main") (ProjectNamespace (Id 13) (ProjectNamespaceFullPath $(mkRelDir "bar")))
                  ]
                )
              ]
          )
        . parTraversePure
        . projectsWithoutExcludesApiInTermsOfProjects
    result = [result1, result2]
    result1 = Result (Id 311) (Name "my-other-project") (ProjectNamespaceFullPath $(mkRelDir "bar")) Running (Just PipelineSourcePush) "main" (Right (Url $$(staticURI "https://my.gitlab.com/pipelines/411")))
    result2 = Result (Id 312) (Name "myProj") (ProjectNamespaceFullPath $(mkRelDir "foo")) Unknown Nothing "" (Left (Url $$(staticURI "https://my.gitlab.com/projects/512/foo")))
    projects =
      M.fromList
        [ ((Id 50, Ref "main"), Pipeline (Id 151) (Ref "main") Successful PipelineSourcePush (Url $$(staticURI "https://sample.de/50"))),
          ((Id 52, Ref "main"), Pipeline (Id 152) (Ref "main") Successful PipelineSourcePush (Url $$(staticURI "https://sample.de/52")))
        ]
    pipelines = M.fromList [((Id 50, Id 151), DetailedPipeline (Id 151) (Ref "main") SuccessfulWithWarnings (Url $$(staticURI "https://detailed.com")))]

pipelinesApiPure :: InterpreterFor PipelinesApi r
pipelinesApiPure = pipelinesApiFromMaps (M.fromList [((Id 311, Ref "main"), Pipeline (Id 411) (Ref "main") Running PipelineSourcePush (Url $$(staticURI "https://my.gitlab.com/pipelines/411")))]) mempty
