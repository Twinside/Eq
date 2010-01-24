{-# LANGUAGE Rank2Types #-}
module EqManips.Algorithm.EmptyMonad( EmptyMonad, fromEmptyMonad, asAMonad )  where

import Control.Applicative

-- | Wrapper type used to use monad function without monad.
-- Implement all instances to be a monad. Equivalent of
-- basic application
newtype EmptyMonad a = EmptyMonad a

instance Functor EmptyMonad where
    {-# INLINE fmap #-}
    fmap f (EmptyMonad a) = EmptyMonad $ f a 
    
instance Applicative EmptyMonad where
    {-# INLINE pure #-}
    pure = EmptyMonad 
    {-# INLINE (<*>) #-}
    (EmptyMonad f) <*> (EmptyMonad a) = EmptyMonad $ f a

instance Monad EmptyMonad where
    {-# INLINE return #-}
    return = EmptyMonad 
    {-# INLINE (>>=) #-}
    (EmptyMonad a) >>= b = b a

-- | a function to unwrap empty monad, just
-- to be able to compose easily.
fromEmptyMonad :: EmptyMonad a -> a
fromEmptyMonad (EmptyMonad a) = a

-- | Perform a pure computation as a monad
asAMonad :: (forall m. (Applicative m, Monad m) => (a -> m b) -> a -> m b) -> (a -> b) -> a -> b
asAMonad f a = fromEmptyMonad . f (EmptyMonad . a)

