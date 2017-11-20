module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Test.Spec.QuickCheck (QCRunnerEffects)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import Test.Data.RPS (rps)
import Test.Data.Strategy (strategy)

main :: Eff (QCRunnerEffects ()) Unit
main = run [consoleReporter] do
  rps
  strategy


