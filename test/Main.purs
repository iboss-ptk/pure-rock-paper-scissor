module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Test.Spec.QuickCheck (QCRunnerEffects)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import Test.RPS (rps)
import Test.Strategy (strategy)

main :: Eff (QCRunnerEffects ()) Unit
main = run [consoleReporter] do
  rps
  strategy


