module Data.Config.Node
  ( fromEnv
  , fromEnv'
  ) where

import Control.Applicative.Free (foldFreeAp)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Maybe.Trans (MaybeT(..))
import Data.Config (Config, ConfigF(..))
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.String as String
import Node.Process (PROCESS, lookupEnv)
import Prelude

-- | Read configuration from environment variables, with some prefix.
fromEnv
  :: ∀ k eff m
   . MonadEff (process :: PROCESS | eff) m
  => String
  -> Config {name :: String | k}
  ~> MaybeT m
fromEnv p = foldFreeAp (fromEnv' p)

-- | Read configuration from environment variables, with some prefix.
fromEnv'
  :: ∀ k eff m
   . MonadEff (process :: PROCESS | eff) m
  => String
  -> ConfigF {name :: String | k}
  ~> MaybeT m
fromEnv' p (String k next) = next <$> lookupEnv' p k.name pure
fromEnv' p (Int    k next) = next <$> lookupEnv' p k.name Int.fromString
fromEnv' p (Prefix k next) = fromEnv (p <> "_" <> k.name) next

lookupEnv'
  :: ∀ a eff m
   . MonadEff (process :: PROCESS | eff) m
  => String
  -> String
  -> (String -> Maybe a)
  -> MaybeT m a
lookupEnv' p k f = MaybeT $
  liftEff $ lookupEnv (String.toUpper $ p <> "_" <> k) <#> (_ >>= f)
