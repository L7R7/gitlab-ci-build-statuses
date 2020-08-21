{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Env where

import Config
import Core.Lib (GroupId)
import Inbound.Jobs.Inbound.Jobs.Updating (HasDataUpdateInterval)
import RIO

class (HasApiToken env, HasBaseUrl env, HasGroupId env, HasDataUpdateInterval env, HasUiUpdateInterval env) => HasConfig env where
  configL :: SimpleGetter env Config

class HasApiToken env where
  apiTokenL :: SimpleGetter env ApiToken

class HasBaseUrl env where
  baseUrlL :: SimpleGetter env BaseUrl

class HasGroupId env where
  groupIdL :: SimpleGetter env GroupId

class HasUiUpdateInterval env where
  uiUpdateIntervalL :: SimpleGetter env UiUpdateIntervalSeconds
