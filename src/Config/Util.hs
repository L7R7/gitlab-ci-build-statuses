{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MonoLocalBinds #-}

module Config.Util (parseConfig, EnvVariableName (..), ErrorMessage (..)) where

import Barbies
import Control.Lens
import Data.Generic.HKD
import Relude hiding (lookupEnv)
import Text.Printf (PrintfArg, PrintfType)
import Validation

newtype EnvVariableName = EnvVariableName String
  deriving newtype (Eq)
  deriving (PrintfArg) via String

newtype ErrorMessage = ErrorMessage String
  deriving (PrintfArg) via String
  deriving (PrintfType) via String

parseConfig ::
  (ApplicativeB (HKD c), TraversableB (HKD c), Construct Identity c) =>
  HKD c (Const EnvVariableName) ->
  HKD c (Const ErrorMessage) ->
  HKD c Maybe ->
  HKD c (Compose ((->) String) Maybe) ->
  [(EnvVariableName, String)] ->
  Validation (NonEmpty Text) c
parseConfig envVarNames errorMsgs defaults parsers environment =
  runIdentity . construct
    <$> validateConfig errorMsgs (parseConfigWithDefaults environment envVarNames parsers defaults)

validateConfig :: (ApplicativeB b, TraversableB b) => b (Const ErrorMessage) -> b Maybe -> Validation (NonEmpty Text) (b Identity)
validateConfig errMsgs mOpts = bsequence' $ bzipWith (\(Const (ErrorMessage errMsg)) -> maybeToSuccess (fromString errMsg :| [])) errMsgs mOpts

parseConfigWithDefaults :: (ApplicativeB b) => [(EnvVariableName, String)] -> b (Const EnvVariableName) -> b (Compose ((->) String) Maybe) -> b Maybe -> b Maybe
parseConfigWithDefaults env envVarNames parse = bzipWith (<|>) (fromEnv env envVarNames parse)

fromEnv :: (ApplicativeB b) => [(EnvVariableName, String)] -> b (Const EnvVariableName) -> b (Compose ((->) String) Maybe) -> b Maybe
fromEnv env = bzipWith (\s (Compose f) -> lookupEnv env s >>= f)

lookupEnv :: [(EnvVariableName, String)] -> Const EnvVariableName k -> Maybe String
lookupEnv env (Const key) = snd <$> find ((== key) . fst) env
