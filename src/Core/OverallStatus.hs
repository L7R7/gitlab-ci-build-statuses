{-# LANGUAGE NoImplicitPrelude #-}

module Core.OverallStatus
  ( isRunning,
    overallStatus,
    OverallStatus (..),
  )
where

import Core.Lib
import Relude

data OverallStatus
  = OverallSuccessful
  | OverallSuccessfulRunning
  | OverallFailedRunning
  | OverallRunning
  | OverallWarningRunning
  | OverallFailed
  | OverallWarning
  | OverallUnknown
  deriving (Bounded, Enum, Eq, Show)

instance Semigroup OverallStatus where
  OverallUnknown <> s = s
  s <> OverallUnknown = s
  OverallFailedRunning <> _ = OverallFailedRunning
  _ <> OverallFailedRunning = OverallFailedRunning
  OverallFailed <> status | isRunning status = OverallFailedRunning
  OverallFailed <> _ = OverallFailed
  status <> OverallFailed | isRunning status = OverallFailedRunning
  _ <> OverallFailed = OverallFailed
  OverallWarningRunning <> _ = OverallWarningRunning
  _ <> OverallWarningRunning = OverallWarningRunning
  OverallWarning <> status | isRunning status = OverallWarningRunning
  OverallWarning <> _ = OverallWarning
  status <> OverallWarning | isRunning status = OverallWarningRunning
  _ <> OverallWarning = OverallWarning
  OverallSuccessfulRunning <> _ = OverallSuccessfulRunning
  _ <> OverallSuccessfulRunning = OverallSuccessfulRunning
  OverallRunning <> OverallRunning = OverallRunning
  OverallSuccessful <> OverallRunning = OverallSuccessfulRunning
  OverallRunning <> OverallSuccessful = OverallSuccessfulRunning
  OverallSuccessful <> _ = OverallSuccessful

instance Monoid OverallStatus where
  mempty = OverallUnknown

isRunning :: OverallStatus -> Bool
isRunning status
  | status `elem` [OverallSuccessfulRunning, OverallFailedRunning, OverallWarningRunning, OverallRunning] = True
  | otherwise = False

overallStatus :: BuildStatuses -> OverallStatus
overallStatus NoSuccessfulUpdateYet = OverallUnknown
overallStatus (Statuses (_, statuses)) = foldMap (resultToOverall . buildStatus) statuses

resultToOverall :: BuildStatus -> OverallStatus
resultToOverall Unknown = OverallUnknown
resultToOverall Cancelled = OverallFailed
resultToOverall Created = OverallRunning
resultToOverall Failed = OverallFailed
resultToOverall Manual = OverallWarning
resultToOverall Pending = OverallRunning
resultToOverall Preparing = OverallRunning
resultToOverall Running = OverallRunning
resultToOverall Scheduled = OverallRunning
resultToOverall Skipped = OverallWarning
resultToOverall Successful = OverallSuccessful
resultToOverall SuccessfulWithWarnings = OverallWarning
resultToOverall WaitingForResource = OverallWarning
