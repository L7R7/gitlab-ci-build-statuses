{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Env where

import Config
import RIO
import System.Metrics

class (HasApiToken env, HasBaseUrl env, HasGroupId env, HasDataUpdateInterval env, HasUiUpdateInterval env) => HasConfig env where
  configL :: Lens' env Config

class HasStore env where
  storeL :: Lens' env Store

class HasApiToken env where
  apiTokenL :: Lens' env ApiToken

class HasBaseUrl env where
  baseUrlL :: Lens' env BaseUrl

class HasGroupId env where
  groupIdL :: Lens' env GroupId

class HasDataUpdateInterval env where
  dataUpdateIntervalL :: Lens' env DataUpdateIntervalMinutes

class HasUiUpdateInterval env where
  uiUpdateIntervalL :: Lens' env UiUpdateIntervalSeconds

class HasStatuses env a where
  statusesL :: Lens' env (IORef [a])
