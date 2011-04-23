{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE Rank2Types #-}
module Language.Eq.Algorithm.EmptyMonad( fromEmptyMonad, asAMonad )  where

import Control.Applicative
import Control.Monad.Identity

-- | a function to unwrap empty monad, just
-- to be able to compose easily.
fromEmptyMonad :: Identity a -> a
fromEmptyMonad = runIdentity

-- | Perform a pure computation as a monad
asAMonad :: (forall m. (Applicative m, Monad m) => (a -> m b) -> a -> m b) -- ^ Monadic function
         -> (a -> b) -- ^ Pure function
         -> a
         -> b
asAMonad f a = fromEmptyMonad . f (Identity . a)

