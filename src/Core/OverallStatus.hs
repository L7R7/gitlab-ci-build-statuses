{-# LANGUAGE NoImplicitPrelude #-}

module Core.OverallStatus(
    overallStatus,
    OverallStatus (..),
) where

import Core.Lib --(BuildStatuses(..))
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
  OverallFailed <> OverallRunning = OverallFailedRunning
  OverallRunning <> OverallFailed = OverallFailedRunning
  OverallFailed <> OverallWarningRunning = OverallFailedRunning
  OverallFailed <> OverallSuccessfulRunning = OverallFailedRunning
  OverallFailed <> _ = OverallFailed
  OverallWarningRunning <> OverallFailed = OverallFailedRunning
  OverallSuccessfulRunning <> OverallFailed = OverallFailedRunning
  _ <> OverallFailed = OverallFailed
  OverallWarningRunning <> _ = OverallWarningRunning
  _ <> OverallWarningRunning = OverallWarningRunning
  OverallWarning <> OverallRunning = OverallWarningRunning
  OverallRunning <> OverallWarning = OverallWarningRunning
  OverallWarning <> OverallSuccessfulRunning = OverallWarningRunning
  OverallSuccessfulRunning <> OverallWarning = OverallWarningRunning
  OverallWarning <> _ = OverallWarning
  _ <> OverallWarning = OverallWarning
  OverallSuccessfulRunning <> _ = OverallSuccessfulRunning
  _ <> OverallSuccessfulRunning = OverallSuccessfulRunning
  OverallRunning <> OverallRunning = OverallRunning
  OverallSuccessful <> OverallRunning = OverallSuccessfulRunning
  OverallRunning <> OverallSuccessful = OverallSuccessfulRunning
  OverallSuccessful <> _ = OverallSuccessful

instance Monoid OverallStatus where
  mempty = OverallUnknown

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
