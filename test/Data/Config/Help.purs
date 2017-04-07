module Test.Data.Config.Help
  ( main
  ) where

import Control.Monad.Eff.Class (liftEff)
import Data.Config (int, optional, prefix, string)
import Data.Config.Help (help, renderHelp)
import Prelude
import Test.Unit (suite, test)
import Test.Unit.Console (print)

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
    liftEff <<< print <<< ("\n" <> _) <<< renderHelp <<< help $ c''
