module Data.Config.Help
  ( Help(..)
  , renderHelp

  , help
  ) where

import Control.Applicative.Free (analyzeFreeAp)
import Data.Config (Config, ConfigF(..), OptionalF(..))
import Data.Exists (runExists)
import Data.Foldable (foldMap)
import Data.List (List(Nil))
import Data.String as String
import Prelude

--------------------------------------------------------------------------------

data Help = Help Boolean String String (List Help)

renderHelp :: List Help -> String
renderHelp = foldMap (renderHelp' "")

renderHelp' :: String -> Help -> String
renderHelp' indent (Help required name info children) =
     indent
  <> name
  <> padding (20 - String.length indent - String.length name)
  <> optional
  <> info
  <> "\n"
  <> foldMap (renderHelp' $ indent <> "  ") children
  where optional | required  = ""
                 | otherwise = "(optional)" <> " "

padding :: Int -> String
padding = go ""
  where go s n | n <= 0    = s
               | otherwise = go (s <> " ") (n - 1)

--------------------------------------------------------------------------------

help :: ∀ k a. Config {name :: String, info :: String | k} a -> List Help
help = analyzeFreeAp (help' true)

help'
  :: ∀ k a
   . Boolean
  -> ConfigF {name :: String, info :: String | k} a
  -> List Help
help' r (String {name, info} _) = pure $ Help r name info Nil
help' r (Int    {name, info} _) = pure $ Help r name info Nil
help' r (Optional            o) = runExists runOptionalF o
  where
  runOptionalF :: ∀ b. OptionalF _ _ b -> _
  runOptionalF (OptionalF c' _) = analyzeFreeAp (help' false) c'
help' r (Prefix {name, info} c) =
  pure $ Help r name info (analyzeFreeAp (help' true) c)
