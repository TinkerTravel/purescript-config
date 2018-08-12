module Test.Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Test.Data.Config.Help as Data.Config.Help
import Test.Data.Config.Node as Data.Config.Node
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  Data.Config.Help.main
  Data.Config.Node.main
