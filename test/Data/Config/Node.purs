module Test.Data.Config.Node
  ( main
  ) where

import Prelude

import Data.Config (Config, int, optional, prefix, string)
import Data.Config.Node (fromEnv)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Process (setEnv)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

data Triple a b c = Triple a b c
derive instance eqTriple :: (Eq a, Eq b, Eq c) => Eq (Triple a b c)
instance showTriple :: (Show a, Show b, Show c) => Show (Triple a b c) where
  show (Triple a b c) =
    "(Triple " <> show a <> " " <> show b <> " " <> show c <> ")"

main :: TestSuite
main = suite "Data.Config.Node" do
  test "pure" do
    let c = pure unit
    assert (Right unit) "pure" c
  test "ap" do
    let c = Triple <$> string {name: "a"}
                   <*> optional (int {name: "b"})
                   <*> prefix {name: "c"} (string {name: "d"})

    liftEffect $ setEnv "TEST_AP_A" "foo"
    assert (Left (Set.fromFoldable ["TEST_AP_C_D"])) "ap" c

    liftEffect do
      setEnv "TEST_AP_A"   "foo"
      setEnv "TEST_AP_C_D" "bar"
    assert (Right (Triple "foo" Nothing "bar")) "ap" c

    liftEffect $ setEnv "TEST_AP_B" "42"
    assert (Right (Triple "foo" (Just 42) "bar")) "ap" c

-- assert :: forall a. Eq a => Show a => a -> a -> Test
assert
  :: forall a r
   . Eq a
  => Show a
  => Either (Set String) a
  -> String
  -> Config { name :: String | r } a
  -> Aff Unit
assert v k c = Assert.equal v =<< fromEnv ("test_" <> k) c
