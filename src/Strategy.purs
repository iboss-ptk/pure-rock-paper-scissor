module Strategy where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.List (List, head)
import Data.Maybe (Maybe(..))
import RPS (RPS(..), Result(..), Round(..), against, whatCanBeat)

nextThrow :: RPS -> List Round -> RPS
nextThrow randomly rounds = case head rounds of
  Nothing ->
    choose randomly
  Just (Round mine opponent's) ->
    case mine `against` opponent's of
      Win  -> choose opponent's
      Lose -> choose whatCanBeat opponent's
      Tie  -> choose randomly
  where
    choose = id

randomRPS :: ∀ e. Eff ( random ∷ RANDOM | e ) RPS
randomRPS = do
  nums <- randomInt 1 3
  pure $ case nums of
    1 -> Rock
    2 -> Paper
    _ -> Scissor
  
