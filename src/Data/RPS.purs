module Data.RPS where 

import Prelude

import Data.Foldable (foldMap)
import Data.Generic (class Generic, gShow)
import Data.List (List)
import Data.Monoid (class Monoid)

-- data type declaration

data RPS = Rock | Paper | Scissors
data Round = Round RPS RPS
data Result = Win | Lose | Tie
data Score = Score Int Int Int


-- type class instances

derive instance genericRPS :: Generic RPS
derive instance eqRPS :: Eq RPS
instance showRPS :: Show RPS where
  show = gShow

derive instance genericResult :: Generic Result
derive instance eqResult :: Eq Result
instance showResult :: Show Result where
  show = gShow

derive instance genericScore :: Generic Score
derive instance eqScore :: Eq Score
instance showScore :: Show Score where
  show = gShow

instance semigroupScore :: Semigroup Score where
  append (Score a b c) (Score a' b' c') = Score (a + a') (b + b') (c + c')

instance monoidScore :: Monoid Score where
  mempty = Score 0 0 0

derive instance genericRound :: Generic Round
instance showRound :: Show Round where
  show = gShow

-- module functions

whatCanBeat :: RPS -> RPS
whatCanBeat Rock = Paper
whatCanBeat Paper = Scissors
whatCanBeat Scissors = Rock

whatLoseTo :: RPS -> RPS
whatLoseTo = whatCanBeat >>> whatCanBeat

against :: RPS -> RPS -> Result
against rps1 rps2 =
  if rps1 == rps2 then
    Tie
  else if rps1 == whatCanBeat rps2 then
    Win
  else
    Lose

roundScore :: Round -> Score
roundScore (Round rps1 rps2) =
  case rps1 `against` rps2 of
    Win  -> Score 1 0 0
    Lose -> Score 0 0 1
    Tie  -> Score 0 1 0

totalScore :: List Round -> Score
totalScore = foldMap roundScore

