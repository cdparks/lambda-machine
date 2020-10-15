module Test.Main
  ( main
  ) where

import Lambda.Prelude

import Effect.Aff (launchAff_)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ do
  specs <- discover "Test\\..*Spec"
  runSpec [consoleReporter] specs
