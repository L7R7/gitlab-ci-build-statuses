module Core.OverallStatus
  ( isRunning,
    overallStatus,
    OverallStatus (..),
  )
where

import Core.Lib (BuildStatus, BuildStatuses (..), Result (buildStatus))
import qualified Core.Lib as B (BuildStatus (..))
import Relude

data OverallStatus
  = Successful
  | SuccessfulRunning
  | FailedRunning
  | Running
  | WarningRunning
  | Failed
  | Warning
  | Unknown
  deriving (Bounded, Enum, Eq, Show)

instance Semigroup OverallStatus where
  Unknown <> s = s
  s <> Unknown = s
  FailedRunning <> _ = FailedRunning
  _ <> FailedRunning = FailedRunning
  Failed <> status | isRunning status = FailedRunning
  Failed <> _ = Failed
  status <> Failed | isRunning status = FailedRunning
  _ <> Failed = Failed
  WarningRunning <> _ = WarningRunning
  _ <> WarningRunning = WarningRunning
  Warning <> status | isRunning status = WarningRunning
  Warning <> _ = Warning
  status <> Warning | isRunning status = WarningRunning
  _ <> Warning = Warning
  SuccessfulRunning <> _ = SuccessfulRunning
  _ <> SuccessfulRunning = SuccessfulRunning
  Running <> Running = Running
  Successful <> Running = SuccessfulRunning
  Running <> Successful = SuccessfulRunning
  Successful <> _ = Successful

instance Monoid OverallStatus where
  mempty = Unknown

isRunning :: OverallStatus -> Bool
isRunning status
  | status `elem` [SuccessfulRunning, FailedRunning, WarningRunning, Running] = True
  | otherwise = False

overallStatus :: BuildStatuses -> OverallStatus
overallStatus NoSuccessfulUpdateYet = Unknown
overallStatus (Statuses (_, statuses)) = foldMap (resultToOverall . buildStatus) statuses

resultToOverall :: BuildStatus -> OverallStatus
resultToOverall B.Unknown = Unknown
resultToOverall B.Cancelled = Failed
resultToOverall B.Created = Running
resultToOverall B.Failed = Failed
resultToOverall B.Manual = Warning
resultToOverall B.Pending = Running
resultToOverall B.Preparing = Running
resultToOverall B.Running = Running
resultToOverall B.Scheduled = Running
resultToOverall B.Skipped = Warning
resultToOverall B.Successful = Successful
resultToOverall B.SuccessfulWithWarnings = Warning
resultToOverall B.WaitingForResource = Warning
