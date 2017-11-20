module Test.Data.Strategy (strategy) where

import Prelude

import Control.Monad.Eff.Random (RANDOM)
import Control.Plus (empty)
import Data.List (singleton)
import Data.RPS (RPS(..), Round(..), whatCanBeat)
import Data.Strategy (nextThrow)
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Util.Generator (genLoseRound, genTieRound, genWinRound)


strategy :: ∀ e. Spec ( random ∷ RANDOM | e ) Unit
strategy =
  describe "Strategy" do
    describe "nextThrow" do
      it "random when it's the first round" do
          nextThrow fakeRandom empty `shouldEqual` fakeRandom

      it "random when it ties" do
         quickCheck $ do
          round <- genTieRound
          pure $ nextThrow fakeRandom (singleton round) === fakeRandom

      it "choose what can beat current opponent's throw when it lose" do
         quickCheck $ do
          round@(Round own opponent's) <- genLoseRound
          pure $ nextThrow fakeRandom (singleton round) === whatCanBeat opponent's

      it "choose current opponent's throw when it win" do
         quickCheck $ do
          round@(Round own opponent's) <- genWinRound 
          pure $ nextThrow fakeRandom (singleton round) === opponent's
    where
      fakeRandom = Rock




