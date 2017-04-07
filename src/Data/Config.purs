-- | Applicative configuration DSL.
module Data.Config
  ( Config
  , ConfigF(..)
  , string
  , int
  , prefix
  ) where

import Control.Applicative.Free (FreeAp, liftFreeAp)
import Prelude

type Config k = FreeAp (ConfigF k)

data ConfigF k a
  = String k (String -> a)
  | Int    k (Int    -> a)
  | Prefix k (Config k a)

-- | A string setting that can contain any text.
string :: ∀ k. k -> Config k String
string = liftFreeAp <<< String `flip` id

-- | An integer setting.
int :: ∀ k. k -> Config k Int
int = liftFreeAp <<< Int `flip` id

-- | A setting with some prefix.
prefix :: ∀ k a. k -> Config k a -> Config k a
prefix = (liftFreeAp <<< _) <<< Prefix
