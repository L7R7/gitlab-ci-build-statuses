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
    describe "Semigroup instance" $ do
      it "adheres to identity" $ hedgehog $ semigroupIdentity overallStatusGen
      it "adheres to associativity" $ hedgehog $ semigroupAssociativity overallStatusGen
    describe "Monoid instance" $ do
      it "adheres to left identity" $ hedgehog $ monoidLeftIdentity overallStatusGen
      it "adheres to right identity" $ hedgehog $ monoidRightIdentity overallStatusGen
      it "mappend behaves like (<>)" $ hedgehog $ monoidMappendCombine overallStatusGen
    describe "folding" $ do
      it "(<>) is commutative" $
        hedgehog $ commutative overallStatusGen
      it "folding respects the hierarchy of the statuses" $ hedgehog resultToOverallProps

resultToOverallProps :: Monad m => PropertyT m ()
resultToOverallProps = do
  statuses <- forAll $ Gen.list (Range.linear 0 100) overallStatusGen
  let result = fold statuses
  case result of
    Successful -> assert $ all (== Successful) statuses
    Unknown -> assert $ all (== Unknown) statuses
    SuccessfulRunning ->
      assert $
        all (`elem` [Successful, SuccessfulRunning, Running, Unknown]) statuses
          && atLeastOneOf [SuccessfulRunning, Running] statuses
    Failed ->
      assert $
        all (`elem` [Successful, Failed, Warning, Unknown]) statuses
          && noneOf [Running, SuccessfulRunning, WarningRunning, FailedRunning] statuses
          && elem Failed statuses
    FailedRunning ->
      assert $
        all (`elem` [Successful, SuccessfulRunning, Failed, FailedRunning, Warning, WarningRunning, Running, Unknown]) statuses
          && atLeastOneOf [Failed, FailedRunning] statuses
          && atLeastOneOf [SuccessfulRunning, FailedRunning, WarningRunning, Running] statuses
    Warning ->
      assert $
        all (`elem` [Successful, Warning, Unknown]) statuses
          && noneOf [Running, SuccessfulRunning, WarningRunning, FailedRunning] statuses
          && elem Warning statuses
    WarningRunning ->
      assert $
        all (`elem` [Successful, SuccessfulRunning, Warning, WarningRunning, Running, Unknown]) statuses
          && atLeastOneOf [Warning, WarningRunning] statuses
          && atLeastOneOf [SuccessfulRunning, WarningRunning, Running] statuses
    Running ->
      assert $
        all (`elem` [Running, Unknown]) statuses
          && elem Running statuses

atLeastOneOf :: (Functor t, Foldable t, Eq a) => t a -> [a] -> Bool
atLeastOneOf mustIncludes as = or ((`elem` as) <$> mustIncludes)

noneOf :: (Functor t, Foldable t, Eq a) => t a -> [a] -> Bool
noneOf mustNotIncludes as = and ((`notElem` as) <$> mustNotIncludes)

overallStatusGen :: Gen OverallStatus
overallStatusGen = Gen.enumBounded

semigroupIdentity :: (Monad m, Show a, Eq a, Monoid a) => Gen a -> PropertyT m ()
semigroupIdentity gen = do
  a <- forAll gen
  a <> a === a

semigroupAssociativity :: (Monad m, Show a, Eq a, Semigroup a) => Gen a -> PropertyT m ()
semigroupAssociativity gen = do
  a <- forAll gen
  b <- forAll gen
  c <- forAll gen
  (a <> b) <> c === a <> (b <> c)

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

commutative :: (Monad m, Show a, Eq a, Semigroup a) => Gen a -> PropertyT m ()
commutative gen = do
  s1 <- forAll gen
  s2 <- forAll gen
  s1 <> s2 === s2 <> s1
