{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Inbound.HTTP.Runners.Html
  ( API,
    template,
  )
where

import Config.Backbone
import Config.Config
import Core.Runners
import Core.Shared (DataUpdateIntervalSeconds (DataUpdateIntervalSeconds), Ref (Ref))
import Data.Map (toList)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime)
import Inbound.HTTP.Util (AutoRefresh (Refresh))
import Polysemy
import Polysemy.Time (Time)
import qualified Polysemy.Time as Time
import Relude
import Servant (Get, QueryFlag, (:>))
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A hiding (icon, name)

type API = "jobs" :> QueryFlag "norefresh" :> Get '[HTML] H.Html

template :: (Member RunnersJobsApi r, Member (Time UTCTime d) r) => DataUpdateIntervalSeconds -> UiUpdateIntervalSeconds -> GitCommit -> AutoRefresh -> Sem r Html
template dataUpdateInterval uiUpdateInterval gitCommit autoRefresh = do
  now <- Time.now
  template' now dataUpdateInterval uiUpdateInterval gitCommit autoRefresh <$> getJobs

template' :: UTCTime -> DataUpdateIntervalSeconds -> UiUpdateIntervalSeconds -> GitCommit -> AutoRefresh -> RunnersJobs -> Html
template' now dataUpdateInterval uiUpdateInterval gitCommit autoRefresh buildStatuses = do
  pageHeader uiUpdateInterval gitCommit autoRefresh buildStatuses
  pageBody dataUpdateInterval now buildStatuses

pageHeader :: UiUpdateIntervalSeconds -> GitCommit -> AutoRefresh -> RunnersJobs -> Html
pageHeader (UiUpdateIntervalSeconds updateInterval) gitCommit autoRefresh runnersJobs =
  docTypeHtml ! lang "en" $
    H.head $
      do
        meta ! charset "UTF-8"
        when (autoRefresh == Refresh) $ meta ! httpEquiv "Refresh" ! content (toValue updateInterval)
        H.title "Running jobs per runner"
        link ! rel "icon" ! type_ "image/png" ! href ("static/" <> faviconPrefix runnersJobs <> "-favicon.ico")
        link ! rel "stylesheet" ! type_ "text/css" ! href "static/normalize-d6d444a732.css"
        link ! rel "stylesheet" ! type_ "text/css" ! href "static/jobs.css"
        script ! type_ "text/javascript" ! src "static/script-909ec6a089.js" $ mempty
        textComment . toText $ ("Version: " <> show gitCommit :: String)

faviconPrefix :: RunnersJobs -> AttributeValue
faviconPrefix (RunnersJobs (_, jobs)) | not (all null jobs) = "running"
faviconPrefix _ = "idle"

pageBody :: DataUpdateIntervalSeconds -> UTCTime -> RunnersJobs -> Html
pageBody dataUpdateInterval now runnersJobs = H.body $ runnersJobsToHtml runnersJobs <> lastUpdatedToHtml dataUpdateInterval now runnersJobs

runnersJobsToHtml :: RunnersJobs -> Html
runnersJobsToHtml NoSuccessfulUpdateYet = H.div ! class_ "job no-successful-update" $ p "There was no successful update yet"
runnersJobsToHtml (RunnersJobs (_, runners)) | null runners = H.div ! class_ "job empty-results" $ p "No online runners found"
runnersJobsToHtml (RunnersJobs (_, runners)) = toHtml $ runnerJobsToHtml <$> Data.Map.toList runners

runnerJobsToHtml :: (Runner, [Job]) -> Html
runnerJobsToHtml (runner, jobs) = H.div ! class_ "runner-container" $ runnerToHtml runner <> (section ! class_ "jobs" $ jobsToHtml jobs)

runnerToHtml :: Runner -> Html
runnerToHtml Runner {..} = H.div ! class_ "runner-info" $ "#" <> toHtml runnerId <> " - " <> toHtml runnerName <> " - @" <> toHtml runnerIpAddress

jobsToHtml :: [Job] -> Html
jobsToHtml [] = H.div ! class_ "job empty" $ p "No running jobs"
jobsToHtml jobs = foldl' (\acc j -> acc <> jobToHtml j) mempty jobs

jobToHtml :: Job -> Html
jobToHtml Job {..} = a ! href (toValue jobWebUrl) ! target "_blank" ! class_ "job" $ do
  H.div ! class_ "job-id" $ "#" <> toHtml jobId
  H.div ! class_ "project-name" $ toHtml jobProjectName
  H.div ! class_ "job-ref" $ truncateRef jobRef
  H.div $ toHtml jobStage <> " > " <> toHtml jobName

deriving newtype instance ToMarkup Stage

deriving newtype instance ToMarkup IpAddress

truncateRef :: Ref -> Html
truncateRef (Ref ref) | T.length ref <= 40 = toHtml ref
truncateRef (Ref ref) = toHtml $ T.take 17 ref <> "..." <> T.drop (T.length ref - 20) ref

lastUpdatedToHtml :: DataUpdateIntervalSeconds -> UTCTime -> RunnersJobs -> Html
lastUpdatedToHtml _ _ NoSuccessfulUpdateYet = mempty
lastUpdatedToHtml (DataUpdateIntervalSeconds updateInterval) now (RunnersJobs (lastUpdate, _)) = H.div ! class_ classes ! staleDataTitle $
  H.div $ do
    p $ "Last Update at: " <> (H.span ! A.id "update-timestamp" $ toHtml (unwords [toText $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" lastUpdate, "UTC"]))
  where
    lastUpdateTooOld = diffUTCTime now lastUpdate > fromIntegral (3 * updateInterval)
    staleDataTitle
      | lastUpdateTooOld = A.title "data is stale. Please check the logs"
      | otherwise = mempty
    classes
      | lastUpdateTooOld = "job timestamp cancelled"
      | otherwise = "job timestamp"
