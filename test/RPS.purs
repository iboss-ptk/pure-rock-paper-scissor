module Test.RPS (rps) where

import Prelude

import Control.Monad.Eff.Random (RANDOM)
import Data.List (List, fromFoldable)
import Data.NonEmpty (NonEmpty(..))
import RPS (RPS(..), Result(..), Round(..), Score(..), against, dominantOf, totalScore)
import Test.QuickCheck ((===))
import Test.QuickCheck.Gen (Gen, chooseInt, elements, shuffle, vectorOf)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

rps :: ∀ e. Spec ( random ∷ RANDOM | e ) Unit
rps =
  describe "RPS" do
    describe "dominantOf" do
       it "returns initial argument when applied three times" do
        quickCheck $ do
          x <- genRPS
          pure $ dominantOf (dominantOf (dominantOf x)) === x

    describe "against" do
       it "tie when both side are the same" do
        quickCheck $ do
           x <- genRPS
           pure $ x `against` x === Tie

       it "win when first rps is a dominant of the second" do
        quickCheck $ do
           x <- genRPS
           pure $ dominantOf x `against` x === Win

       it "lose when second rps is a dominant of the other" do
        quickCheck $ do
           x <- genRPS
           pure $ x `against` dominantOf x === Lose

    describe "totalScore" do
       it "count scores for only wins and loses" do
        quickCheck $ do
           wins <- chooseInt 0 10
           loses <- chooseInt 0 10
           ties <- chooseInt 0 10
           rounds <- genRounds wins loses ties
           pure $ totalScore rounds === Score wins loses


-- generators

genRPS :: Gen RPS
genRPS = elements $ NonEmpty Rock [ Paper, Scissor ]

genWinRound :: Gen Round
genWinRound = do
  x <- genRPS
  pure $ Round (dominantOf x) x

genLoseRound :: Gen Round
genLoseRound = do
  x <- genRPS
  pure $ Round x (dominantOf x)

genTieRound :: Gen Round
genTieRound = do
  x <- genRPS
  pure $ Round x x

genRounds :: Int -> Int -> Int -> Gen (List Round)
genRounds wins loses ties =
    map fromFoldable $ do
       winRounds <- vectorOf wins genWinRound
       loseRounds <- vectorOf loses genLoseRound
       tieRounds <- vectorOf ties genTieRound
       shuffle $ winRounds <> loseRounds <> tieRounds

