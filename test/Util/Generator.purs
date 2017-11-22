module Util.Generator where

import Prelude

import Test.QuickCheck.Gen (Gen, elements, shuffle, vectorOf)
import Data.RPS (RPS(Rock, Paper, Scissors), Round(Round), whatCanBeat)
import Data.NonEmpty (NonEmpty(..))
import Data.List (List, fromFoldable)

genRPS :: Gen RPS
genRPS = elements $ NonEmpty Rock [ Paper, Scissors ]

genWinRound :: Gen Round
genWinRound = do
  x <- genRPS
  pure $ Round (whatCanBeat x) x

genLoseRound :: Gen Round
genLoseRound = do
  x <- genRPS
  pure $ Round x (whatCanBeat x)

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

