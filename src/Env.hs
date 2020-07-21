{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Env where

import Config
import Core.Lib (GroupId, HasDataUpdateInterval)
import RIO

class (HasApiToken env, HasBaseUrl env, HasGroupId env, HasDataUpdateInterval env, HasUiUpdateInterval env) => HasConfig env where
  configL :: Lens' env Config

class HasApiToken env where
  apiTokenL :: Lens' env ApiToken

class HasBaseUrl env where
  baseUrlL :: Lens' env BaseUrl

class HasGroupId env where
  groupIdL :: Lens' env GroupId

class HasUiUpdateInterval env where
  uiUpdateIntervalL :: Lens' env UiUpdateIntervalSeconds
