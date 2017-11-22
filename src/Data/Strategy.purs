module Data.Strategy where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, random, randomInt)
import Data.List (List, head, take)
import Data.Maybe (Maybe(..))
import Data.RPS (RPS(..), Result(..), Round(..), Score(..), against, totalScore, whatCanBeat)

-- use player vs bot instead
nextThrow :: RPS -> List Round -> RPS
nextThrow randomly rounds = case head rounds of
  Nothing ->
    choose randomly
  Just (Round bot's player's) ->
    case bot's `against` player's of
      Win  -> choose player's
      Lose -> choose whatCanBeat player's
      Tie  -> choose randomly
  where
    choose = id

randomRPS :: ∀ e. Eff ( random ∷ RANDOM | e ) RPS
randomRPS = do
  nums <- randomInt 1 3
  pure $ case nums of
    1 -> Rock
    2 -> Paper
    _ -> Scissors
  
nextThrowWithRandom :: ∀ e. List Round -> Eff ( random ∷ RANDOM | e ) RPS
nextThrowWithRandom rounds = do
  nextRandomRPS <- randomRPS
  percentile <- random

  let shouldBeStrategic = percentile < strategicThrowPossibility
  let isPlayerCounterStrategy =
       case (take observingRoundCount rounds # totalScore) of
        Score _ _ playerWins -> playerWins > playerWinsThreshold

  pure $
    if shouldBeStrategic then
      if isPlayerCounterStrategy then
        whatCanBeat >>> whatCanBeat $ nextThrow nextRandomRPS rounds
      else
        nextThrow nextRandomRPS rounds
    else
      nextRandomRPS

  where
    strategicThrowPossibility = 0.7
    observingRoundCount = 10
    playerWinsThreshold = 5


