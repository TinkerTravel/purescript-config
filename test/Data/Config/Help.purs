module Test.Data.Config.Help
  ( main
  ) where

import Prelude

import Data.Config (int, optional, prefix, string)
import Data.Config.Help (help, renderHelp)
import Effect.Class (liftEffect)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Console (print)

main :: TestSuite
main = suite "Data.Config.Node" do
  test "example" do
    let c = (\_ _ _ _ -> unit)
            <$> string   {name: "foo", info: "a foo"}
            <*> int      {name: "bar", info: "a bar"}
            <*> optional (string {name: "baz", info: "a baz"})
    let c' = (\_ _ -> unit)
             <$> c
             <*> prefix {name: "qux", info: "a qux"} c
    let c'' = (\_ _ -> unit)
              <$> c'
              <*> optional (prefix {name: "kek", info: "a kek"} c')
    liftEffect <<< print <<< ("\n" <> _) <<< renderHelp <<< help $ c''
