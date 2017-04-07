module Test.Data.Config.Node
  ( main
  ) where

import Control.Monad.Eff.Class (liftEff)
import Data.Config (int, optional, prefix, string)
import Data.Config.Node (fromEnv)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set as Set
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
    assert (Right unit) "pure" c
  test "ap" do
    let c = Triple <$> string {name: "a"}
                   <*> optional (int {name: "b"})
                   <*> prefix {name: "c"} (string {name: "d"})

    liftEff $ setEnv "TEST_AP_A" "foo"
    assert (Left (Set.fromFoldable ["TEST_AP_C_D"])) "ap" c

    liftEff do
      setEnv "TEST_AP_A"   "foo"
      setEnv "TEST_AP_C_D" "bar"
    assert (Right (Triple "foo" Nothing "bar")) "ap" c

    liftEff $ setEnv "TEST_AP_B" "42"
    assert (Right (Triple "foo" (Just 42) "bar")) "ap" c

assert v k c = Assert.equal v =<< fromEnv ("test_" <> k) c
