module Test.Data.Config.Node
  ( main
  ) where

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Maybe.Trans (runMaybeT)
import Data.Config (int, prefix, string)
import Data.Config.Node (fromEnv)
import Data.Maybe (Maybe(..))
import Node.Process (setEnv)
import Prelude
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert

data Triple a b c = Triple a b c
derive instance eqTriple :: (Eq a, Eq b, Eq c) => Eq (Triple a b c)
instance showTriple :: (Show a, Show b, Show c) => Show (Triple a b c) where
  show (Triple a b c) =
    "(Triple " <> show a <> " " <> show b <> " " <> show c <> ")"

main = suite "Data.Config.Node" do
  test "pure" do
    let c = pure unit
    assert (Just unit) "pure" c
  test "ap" do
    let c = Triple <$> string {name: "a"}
                   <*> int    {name: "b"}
                   <*> prefix {name: "c"} (string {name: "d"})
    assert Nothing "ap" c
    liftEff do
      setEnv "TEST_AP_A"   "foo"
      setEnv "TEST_AP_B"   "42"
      setEnv "TEST_AP_C_D" "bar"
    assert (Just (Triple "foo" 42 "bar")) "ap" c

assert v k c = Assert.equal v =<< runMaybeT (fromEnv ("test_" <> k) c)
