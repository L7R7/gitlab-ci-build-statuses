{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UseCases.Shared
  ( findProjects,
  )
where

import Core.BuildStatuses
import Core.Effects (Logger, addContext, logWarn)
import Core.Shared
import Data.Aeson (ToJSON)
import Polysemy
import qualified Polysemy.Reader as R
import Relude

findProjects :: (Member ProjectsApi r, Member Logger r, Member (R.Reader (Id Group)) r, Member (R.Reader [Id Project]) r) => Sem r [Project]
findProjects = do
  groupId <- R.ask
  excludeList <- R.ask
  addContext "groupId" groupId $ do
    result <- getProjects groupId
    case result of
      Left err -> [] <$ logWarn (unwords ["Couldn't load projects. Error was", show err])
      Right ps -> do
        let orphansInExcludeList = filter (\pId -> pId `notElem` (projectId <$> ps)) excludeList
        unless (null orphansInExcludeList) $ addContext "orphanProjects" (show @String orphansInExcludeList) $ logWarn "There are projects on the exclude list that are not included in the result. This is probably a configuration error"
        let filtered = filter (\p -> projectId p `notElem` excludeList) ps
        pure filtered

deriving newtype instance ToJSON (Id a)
