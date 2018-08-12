module Data.Config.Node
  ( fromEnv
  ) where

import Control.Applicative.Free (foldFreeAp)
import Effect.Class (class MonadEffect, liftEffect)
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
import Node.Process (lookupEnv)
import Prelude

-- | Read configuration from environment variables, with some prefix.
fromEnv
  :: ∀ k m a
   . MonadEffect m
  => String
  -> Config {name :: String | k} a
  -> m (Either (Set String) a)
fromEnv p c = map (unV Left Right) <<< unwrap $ fromEnv' p c

fromEnv'
  :: ∀ k m
   . MonadEffect m
  => String
  -> Config {name :: String | k}
  ~> Compose m (V (Set String))
fromEnv' p = foldFreeAp (fromEnv'' p)

fromEnv''
  :: ∀ k m
   . MonadEffect m
  => String
  -> ConfigF {name :: String | k}
  ~> Compose m (V (Set String))
fromEnv'' p (String k next) = next <$> lookupEnv' p k.name pure
fromEnv'' p (Int    k next) = next <$> lookupEnv' p k.name Int.fromString
fromEnv'' p (Optional next) = runExists runOptionalF next
  where
  runOptionalF :: ∀ a b. OptionalF { name :: String | k } a b -> Compose m (V (Set String)) a
  runOptionalF (OptionalF c l) = wrap $
    fromEnv p c
    <#> either (const Nothing) Just
    <#> coerceSymm l
    <#> pure
fromEnv'' p (Prefix k next) = fromEnv' (p <> "_" <> k.name) next

lookupEnv'
  :: ∀ a m
   . MonadEffect m
  => String
  -> String
  -> (String -> Maybe a)
  -> Compose m (V (Set String)) a
lookupEnv' p k f =
  let l = String.toUpper $ p <> "_" <> k
      e = liftEffect $ lookupEnv l <#> (_ >>= f)
  in wrap $ e <#> maybe (invalid $ Set.singleton l) pure
