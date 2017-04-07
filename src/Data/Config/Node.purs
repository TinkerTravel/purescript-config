module Data.Config.Node
  ( fromEnv
  , fromEnv'
  , fromEnv''
  ) where

import Control.Applicative.Free (foldFreeAp)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Data.Config (Config, ConfigF(..), OptionalF(..))
import Data.Either (Either(..), either)
import Data.Exists (runExists)
import Data.Functor.Compose (Compose)
import Data.Int as Int
import Data.Leibniz (coerceSymm)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Validation.Semigroup (V, invalid, unV)
import Node.Process (PROCESS, lookupEnv)
import Prelude

-- | Read configuration from environment variables, with some prefix.
fromEnv
  :: ∀ k eff m a
   . MonadEff (process :: PROCESS | eff) m
  => String
  -> Config {name :: String | k} a
  -> m (Either (Set String) a)
fromEnv p c = map (unV Left Right) <<< unwrap $ fromEnv' p c

-- | Read configuration from environment variables, with some prefix.
fromEnv'
  :: ∀ k eff m
   . MonadEff (process :: PROCESS | eff) m
  => String
  -> Config {name :: String | k}
  ~> Compose m (V (Set String))
fromEnv' p = foldFreeAp (fromEnv'' p)

-- | Read configuration from environment variables, with some prefix.
fromEnv''
  :: ∀ k eff m
   . MonadEff (process :: PROCESS | eff) m
  => String
  -> ConfigF {name :: String | k}
  ~> Compose m (V (Set String))
fromEnv'' p (String k next) = next <$> lookupEnv' p k.name pure
fromEnv'' p (Int    k next) = next <$> lookupEnv' p k.name Int.fromString
fromEnv'' p (Optional next) = runExists runOptionalF next
  where
  runOptionalF :: ∀ b. OptionalF _ _ b -> _
  runOptionalF (OptionalF c l) = wrap $
    fromEnv p c
    <#> either (const $ coerceSymm l Nothing)
               (coerceSymm l <<< Just)
    <#> pure
fromEnv'' p (Prefix k next) = fromEnv' (p <> "_" <> k.name) next

lookupEnv'
  :: ∀ a eff m
   . MonadEff (process :: PROCESS | eff) m
  => String
  -> String
  -> (String -> Maybe a)
  -> Compose m (V (Set String)) a
lookupEnv' p k f =
  let l = String.toUpper $ p <> "_" <> k
      e = liftEff $ lookupEnv l <#> (_ >>= f)
  in wrap $ e <#> maybe (invalid $ Set.singleton l) pure
