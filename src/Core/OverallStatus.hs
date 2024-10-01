module Core.OverallStatus
  ( isRunning,
    overallStatus,
    OverallStatus (..),
  )
where

import Core.BuildStatuses (BuildStatuses (..), Result (buildStatus))
import Gitlab.Job (JobStatus)
import Gitlab.Job qualified as J (JobStatus (..))
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
  deriving stock (Bounded, Enum, Eq, Show)

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

resultToOverall :: JobStatus -> OverallStatus
resultToOverall J.Unknown = Unknown
resultToOverall J.Cancelled = Failed
resultToOverall J.Created = Running
resultToOverall J.Failed = Failed
resultToOverall J.Manual = Warning
resultToOverall J.Pending = Running
resultToOverall J.Preparing = Running
resultToOverall J.Running = Running
resultToOverall J.Scheduled = Running
resultToOverall J.Skipped = Warning
resultToOverall J.Successful = Successful
resultToOverall J.SuccessfulWithWarnings = Warning
resultToOverall J.WaitingForResource = Warning
