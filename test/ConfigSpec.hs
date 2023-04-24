{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ConfigSpec where

import Config.Config
import Core.Shared
import Katip
import Network.URI.Static
import Relude
import Test.Hspec
import Validation

spec :: Spec
spec = do
  describe "config parsing" $ do
    it "fails for empty environment" $
      parseConfigFromEnv []
        `shouldBe` Failure
          ( "Gitlab API Token is missing (set it via GCB_GITLAB_API_TOKEN)"
              :| [ "Group ID is missing (set it via GCB_GITLAB_GROUP_ID)",
                   "Gitlab base URL is missing (set it via GCB_GITLAB_BASE_URL)"
                 ]
          )

    it "works when all required values are provided" $
      parseConfigFromEnv mandatoryConfig
        `shouldBe` Success expectedConfig

    it "allows setting several group IDs" $
      parseConfigFromEnv (("GCB_GITLAB_GROUP_ID", "5,6") : mandatoryConfig)
        `shouldBe` Success (expectedConfig {groupId = Id 5 :| [Id 6]})

    describe "overriding for non-mandatory fields" $ do
      it "should allow overriding data update interval" $
        parseConfigFromEnv (("GCB_DATA_UPDATE_INTERVAL_SECS", "5") : mandatoryConfig)
          `shouldBe` Success (expectedConfig {dataUpdateIntervalSecs = DataUpdateIntervalSeconds 5})
      it "should allow overriding UI update interval" $
        parseConfigFromEnv (("GCB_UI_UPDATE_INTERVAL_SECS", "10") : mandatoryConfig)
          `shouldBe` Success (expectedConfig {uiUpdateIntervalSecs = UiUpdateIntervalSeconds 10})
      it "should allow overriding project cache TTL" $
        parseConfigFromEnv (("GCB_PROJECT_CACHE_TTL_SECS", "5") : mandatoryConfig)
          `shouldBe` Success (expectedConfig {projectCacheTtlSecs = ProjectCacheTtlSeconds 5})
      it "should allow overriding runner cache TTL" $
        parseConfigFromEnv (("GCB_RUNNER_CACHE_TTL_SECS", "5") : mandatoryConfig)
          `shouldBe` Success (expectedConfig {runnerCacheTtlSecs = RunnerCacheTtlSeconds 5})
      it "should allow overriding maximum concurency" $
        parseConfigFromEnv (("GCB_MAX_CONCURRENCY", "5") : mandatoryConfig)
          `shouldBe` Success (expectedConfig {maxConcurrency = MaxConcurrency 5})
      it "should allow overriding whether to include shared projects" $
        parseConfigFromEnv (("GCB_INCLUDE_SHARED_PROJECTS", "exclude") : mandatoryConfig)
          `shouldBe` Success (expectedConfig {includeSharedProjects = Exclude})
      it "should allow overriding whether to enable jobs view" $
        parseConfigFromEnv (("GCB_JOBS_VIEW", "disabled") : mandatoryConfig)
          `shouldBe` Success (expectedConfig {jobsView = Disabled})
      it "should allow overriding the log level" $
        parseConfigFromEnv (("GCB_LOG_LEVEL", "ERROR") : mandatoryConfig)
          `shouldBe` Success (expectedConfig {logLevel = ErrorS})
      describe "project exclude list" $ do
        it "should allow overriding a single project ID" $
          parseConfigFromEnv (("GCB_EXCLUDE_PROJECTS", "12") : mandatoryConfig)
            `shouldBe` Success (expectedConfig {projectExcludeList = [Id 12]})
        it "should allow overriding multiple project IDs" $
          parseConfigFromEnv (("GCB_EXCLUDE_PROJECTS", "12,13") : mandatoryConfig)
            `shouldBe` Success (expectedConfig {projectExcludeList = [Id 12, Id 13]})
        it "should deduplicate the list of IDs" $
          parseConfigFromEnv (("GCB_EXCLUDE_PROJECTS", "12,12") : mandatoryConfig)
            `shouldBe` Success (expectedConfig {projectExcludeList = [Id 12]})
        it "should trim leading and trailing whitespace" $
          parseConfigFromEnv (("GCB_EXCLUDE_PROJECTS", " 12,13,14 , 15 ") : mandatoryConfig)
            `shouldBe` Success (expectedConfig {projectExcludeList = [Id 12, Id 13, Id 14, Id 15]})
        it "should handle empty values" $
          parseConfigFromEnv (("GCB_EXCLUDE_PROJECTS", "") : mandatoryConfig)
            `shouldBe` Success expectedConfig

    it "expects a non empty API token" $ do
      parseConfigFromEnv [("GCB_GITLAB_API_TOKEN", ""), ("GCB_GITLAB_GROUP_ID", "123"), ("GCB_GITLAB_BASE_URL", "https://my.gitlab.com")]
        `shouldBe` Failure ("Gitlab API Token is missing (set it via GCB_GITLAB_API_TOKEN)" :| [])
      parseConfigFromEnv [("GCB_GITLAB_GROUP_ID", "123"), ("GCB_GITLAB_BASE_URL", "https://my.gitlab.com")]
        `shouldBe` Failure ("Gitlab API Token is missing (set it via GCB_GITLAB_API_TOKEN)" :| [])

    it "expects a positive integer for the group ID" $
      parseConfigFromEnv [("GCB_GITLAB_API_TOKEN", "apitoken"), ("GCB_GITLAB_GROUP_ID", "-123"), ("GCB_GITLAB_BASE_URL", "https://my.gitlab.com")]
        `shouldBe` Failure ("Group ID is missing (set it via GCB_GITLAB_GROUP_ID)" :| [])

    it "expects a valid URL for the Gitlab host" $
      parseConfigFromEnv [("GCB_GITLAB_API_TOKEN", "apitoken"), ("GCB_GITLAB_GROUP_ID", "123"), ("GCB_GITLAB_BASE_URL", "this-is-no-url")]
        `shouldBe` Failure ("Gitlab base URL is missing (set it via GCB_GITLAB_BASE_URL)" :| [])
  where
    mandatoryConfig = [("GCB_GITLAB_API_TOKEN", "apitoken"), ("GCB_GITLAB_GROUP_ID", "123"), ("GCB_GITLAB_BASE_URL", "https://my.gitlab.com")]
    expectedConfig =
      Config
        (ApiToken "apitoken")
        (Id 123 :| [])
        (Url $$(staticURI "https://my.gitlab.com"))
        (DataUpdateIntervalSeconds 60)
        (UiUpdateIntervalSeconds 5)
        (ProjectCacheTtlSeconds 3600)
        (RunnerCacheTtlSeconds 300)
        (MaxConcurrency 2)
        Include
        InfoS
        []
        Enabled
