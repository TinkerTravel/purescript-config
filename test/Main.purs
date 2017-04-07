module Test.Main
  ( main
  ) where

import Test.Data.Config.Node as Data.Config.Node
import Test.Unit.Main (runTest)

main = runTest do
  Data.Config.Node.main
