{-# LANGUAGE NoImplicitPrelude #-}

module OverallStatusSpec where

import Core.OverallStatus (OverallStatus (..))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Relude
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = do
  describe "OverallStatus" $ do
    describe "monoid instance" $ do
      it "adheres to left identity" $ hedgehog $ monoidLeftIdentity overallStatusGen
      it "adheres to right identity" $ hedgehog $ monoidRightIdentity overallStatusGen
      it "mappend behaves like (<>)" $ hedgehog $ monoidMappendCombine overallStatusGen
    describe "folding" $ do
      it "(<>) is commutative" $
        hedgehog $ do
          s1 <- forAll overallStatusGen
          s2 <- forAll overallStatusGen
          s1 <> s2 === s2 <> s1
      it "folding respects the hierarchy of the statuses" $ hedgehog resultToOverallProps

resultToOverallProps :: Monad m => PropertyT m ()
resultToOverallProps = do
  statuses <- forAll $ Gen.list (Range.linear 0 100) overallStatusGen
  let result = fold statuses
  case result of
    OverallSuccessful -> assert $ all (== OverallSuccessful) statuses
    OverallUnknown -> assert $ all (== OverallUnknown) statuses
    OverallSuccessfulRunning ->
      assert $
        all (`elem` [OverallSuccessful, OverallSuccessfulRunning, OverallRunning, OverallUnknown]) statuses
          && atLeastOneOf [OverallSuccessfulRunning, OverallRunning] statuses
    OverallFailed ->
      assert $
        all (`elem` [OverallSuccessful, OverallFailed, OverallWarning, OverallUnknown]) statuses
          && noneOf [OverallRunning, OverallSuccessfulRunning, OverallWarningRunning, OverallFailedRunning] statuses
          && elem OverallFailed statuses
    OverallFailedRunning ->
      assert $
        all (`elem` [OverallSuccessful, OverallSuccessfulRunning, OverallFailed, OverallFailedRunning, OverallWarning, OverallWarningRunning, OverallRunning, OverallUnknown]) statuses
          && atLeastOneOf [OverallFailed, OverallFailedRunning] statuses
          && atLeastOneOf [OverallSuccessfulRunning, OverallFailedRunning, OverallWarningRunning, OverallRunning] statuses
    OverallWarning ->
      assert $
        all (`elem` [OverallSuccessful, OverallWarning, OverallUnknown]) statuses
          && noneOf [OverallRunning, OverallSuccessfulRunning, OverallWarningRunning, OverallFailedRunning] statuses
          && elem OverallWarning statuses
    OverallWarningRunning ->
      assert $
        all (`elem` [OverallSuccessful, OverallSuccessfulRunning, OverallWarning, OverallWarningRunning, OverallRunning, OverallUnknown]) statuses
          && atLeastOneOf [OverallWarning, OverallWarningRunning] statuses
          && atLeastOneOf [OverallSuccessfulRunning, OverallWarningRunning, OverallRunning] statuses
    OverallRunning ->
      assert $
        all (`elem` [OverallRunning, OverallUnknown]) statuses
          && elem OverallRunning statuses

atLeastOneOf :: (Functor t, Foldable t, Eq a) => t a -> [a] -> Bool
atLeastOneOf mustIncludes as = or ((`elem` as) <$> mustIncludes)

noneOf :: (Functor t, Foldable t, Eq a) => t a -> [a] -> Bool
noneOf mustNotIncludes as = and ((`notElem` as) <$> mustNotIncludes)

overallStatusGen :: Gen OverallStatus
overallStatusGen = Gen.enumBounded

monoidLeftIdentity :: (Monad m, Show a, Eq a, Monoid a) => Gen a -> PropertyT m ()
monoidLeftIdentity gen = do
  a <- forAll gen
  mempty <> a === a

monoidRightIdentity :: (Monad m, Show a, Eq a, Monoid a) => Gen a -> PropertyT m ()
monoidRightIdentity gen = do
  a <- forAll gen
  a <> mempty === a

monoidMappendCombine :: (Monad m, Show a, Eq a, Monoid a) => Gen a -> PropertyT m ()
monoidMappendCombine gen = do
  a <- forAll gen
  b <- forAll gen
  a <> b === mappend a b
