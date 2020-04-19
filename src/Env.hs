{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Env where

import Config
import qualified Data.Text as T
import Katip
import RIO
import TextShow (TextShow (..), showbCommaSpace)

data App
  = App
      { statuses :: !(IORef [Result]),
        config :: !Config,
        logNamespace :: !Namespace,
        logContext :: !LogContexts,
        logEnv :: !LogEnv
      }

instance Katip (RIO App) where
  getLogEnv = asks logEnv
  localLogEnv f (RIO a) = RIO (local (\s -> s {logEnv = f (logEnv s)}) a)

instance KatipContext (RIO App) where
  getKatipContext = asks logContext
  localKatipContext f (RIO app) = RIO (local (\s -> s {logContext = f (logContext s)}) app)
  getKatipNamespace = asks logNamespace
  localKatipNamespace f (RIO app) = RIO (local (\s -> s {logNamespace = f (logNamespace s)}) app)

class (HasApiToken env, HasBaseUrl env, HasGroupId env, HasDataUpdateInterval env, HasUiUpdateInterval env) => HasConfig env where
  configL :: Lens' env Config

instance HasConfig App where
  configL = lens config (\app iJC -> app {config = iJC})

class HasApiToken env where
  apiTokenL :: Lens' env ApiToken

instance HasApiToken App where
  apiTokenL = lens (apiToken . config) (\app token -> app {config = (config app) {apiToken = token}})

class HasBaseUrl env where
  baseUrlL :: Lens' env BaseUrl

instance HasBaseUrl App where
  baseUrlL = lens (gitlabBaseUrl . config) (\app u -> app {config = (config app) {gitlabBaseUrl = u}})

class HasGroupId env where
  groupIdL :: Lens' env GroupId

instance HasGroupId App where
  groupIdL = lens (groupId . config) (\app g -> app {config = (config app) {groupId = g}})

class HasDataUpdateInterval env where
  dataUpdateIntervalL :: Lens' env DataUpdateIntervalMinutes

instance HasDataUpdateInterval App where
  dataUpdateIntervalL = lens (dataUpdateIntervalMins . config) (\app d -> app {config = (config app) {dataUpdateIntervalMins = d}})

class HasUiUpdateInterval env where
  uiUpdateIntervalL :: Lens' env UiUpdateIntervalSeconds

instance HasUiUpdateInterval App where
  uiUpdateIntervalL = lens (uiUpdateIntervalSecs . config) (\app u -> app {config = (config app) {uiUpdateIntervalSecs = u}})

class HasStatuses env where
  statusesL :: Lens' env (IORef [Result])

instance HasStatuses App where
  statusesL = lens statuses (\app st -> app {statuses = st})

data Result = Result {projId :: Int, name :: T.Text, buildStatus :: BuildStatus, url :: T.Text} deriving (Show)

instance TextShow Result where
  showb (Result i n bs _) = showb i <> showbCommaSpace <> showb n <> showbCommaSpace <> showb bs

data BuildStatus = Unknown | Running | Failed | Cancelled | Pending | Skipped | Successful deriving (Eq, Show, Ord)

instance TextShow BuildStatus where
  showb = showb . show
