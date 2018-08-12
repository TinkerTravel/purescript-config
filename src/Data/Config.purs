-- | Applicative configuration DSL.
module Data.Config
  ( Config
  , ConfigF(..)
  , OptionalF(..)
  , string
  , int
  , optional
  , prefix
  ) where

import Control.Applicative.Free (FreeAp, liftFreeAp)
import Data.Exists (Exists, mkExists)
import Data.Leibniz (type (~))
import Data.Maybe (Maybe)
import Prelude

-- | A configuration description.
type Config k = FreeAp (ConfigF k)

-- | A setting description.
data ConfigF k a
  = String   k (String -> a)
  | Int      k (Int    -> a)
  | Optional   (Exists (OptionalF k a))
  | Prefix   k (Config k a)

-- | Helper type for existential quantification.
data OptionalF k a b
  = OptionalF (Config k b) (a ~ Maybe b)

-- | A string setting that can contain any text.
string :: ∀ k. k -> Config k String
string = liftFreeAp <<< String `flip` identity

-- | An integer setting.
int :: ∀ k. k -> Config k Int
int = liftFreeAp <<< Int `flip` identity

-- | An optional setting.
optional :: ∀ k a. Config k a -> Config k (Maybe a)
optional c = liftFreeAp <<< Optional $ mkExists (OptionalF c identity)

-- | A setting with some prefix.
prefix :: ∀ k a. k -> Config k a -> Config k a
prefix = (liftFreeAp <<< _) <<< Prefix
