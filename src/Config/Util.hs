{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Config.Util (parseConfig) where

import Barbies
import Control.Lens
import Data.Generic.HKD
import Relude hiding (lookupEnv)
import Validation

parseConfig ::
  (ApplicativeB (HKD c), TraversableB (HKD c), Construct Identity c) =>
  HKD c (Const String) ->
  HKD c (Const String) ->
  HKD c Maybe ->
  HKD c (Compose ((->) String) Maybe) ->
  [(String, String)] ->
  Validation (NonEmpty Text) c
parseConfig envVarNames errorMsgs defaults parsers environment =
  runIdentity . construct
    <$> validateConfig errorMsgs (parseConfigWithDefaults environment envVarNames parsers defaults)

validateConfig :: (ApplicativeB b, TraversableB b) => b (Const String) -> b Maybe -> Validation (NonEmpty Text) (b Identity)
validateConfig errMsgs mOpts = bsequence' $ bzipWith (\(Const errMsg) -> maybeToSuccess (fromString errMsg :| [])) errMsgs mOpts

parseConfigWithDefaults :: ApplicativeB b => [(String, String)] -> b (Const String) -> b (Compose ((->) String) Maybe) -> b Maybe -> b Maybe
parseConfigWithDefaults env envVarNames parse = bzipWith (<|>) (fromEnv env envVarNames parse)

fromEnv :: ApplicativeB b => [(String, String)] -> b (Const String) -> b (Compose ((->) String) Maybe) -> b Maybe
fromEnv env = bzipWith (\s (Compose f) -> lookupEnv env s >>= f)

lookupEnv :: [(String, String)] -> Const String k -> Maybe String
lookupEnv env (Const key) = snd <$> find ((== toString key) . fst) env
