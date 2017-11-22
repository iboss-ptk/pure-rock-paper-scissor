module Test.Data.RPS (rps) where

import Prelude

import Control.Monad.Eff.Random (RANDOM)
import Data.RPS (Result(Lose, Win, Tie), Score(Score), against, totalScore, whatCanBeat, whatLoseTo)
import Test.QuickCheck ((===))
import Test.QuickCheck.Gen (chooseInt)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Util.Generator (genRPS, genRounds)

rps :: ∀ e. Spec ( random ∷ RANDOM | e ) Unit
rps =
  describe "RPS" do
    describe "whatCanBeat" do
       it "returns initial argument when applied three times" do
        quickCheck $ do
          x <- genRPS
          pure $ whatCanBeat (whatCanBeat (whatCanBeat x)) === x

    describe "whatLoseTo" do
       it "is an inverse of whatCanBeat" do
        quickCheck $ do
          x <- genRPS
          pure $ whatCanBeat (whatLoseTo x) === x
        quickCheck $ do
          x <- genRPS
          pure $ whatLoseTo (whatCanBeat x) === x

    describe "against" do
       it "tie when both side are the same" do
        quickCheck $ do
           x <- genRPS
           pure $ x `against` x === Tie

       it "win when first rps can beat the second" do
        quickCheck $ do
           x <- genRPS
           pure $ whatCanBeat x `against` x === Win

       it "lose when second rps can beat the first" do
        quickCheck $ do
           x <- genRPS
           pure $ x `against` whatCanBeat x === Lose

    describe "totalScore" do
       it "count scores for only wins and loses" do
        quickCheck $ do
           wins <- chooseInt 0 10
           loses <- chooseInt 0 10
           ties <- chooseInt 0 10
           rounds <- genRounds wins loses ties
           pure $ totalScore rounds === Score wins ties loses


