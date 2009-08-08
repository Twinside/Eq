{-# LANGUAGE Rank2Types #-}
module EqManips.Algorithm.Utils ( EmptyMonad( .. )
                                , biAssocM 
                                , biAssoc
                                , asAMonad
                                , fromEmptyMonad 
                                , treeIfyBinOp 
                                ) where

import Control.Applicative
import EqManips.Types

newtype EmptyMonad a = EmptyMonad a

instance Functor EmptyMonad where
    fmap f (EmptyMonad a) = EmptyMonad $ f a 
    
instance Applicative EmptyMonad where
    pure = EmptyMonad 
    (EmptyMonad f) <*> (EmptyMonad a) = EmptyMonad $ f a

instance Monad EmptyMonad where
    return  = EmptyMonad 
    (EmptyMonad a) >>= b = b a

fromEmptyMonad :: EmptyMonad a -> a
fromEmptyMonad (EmptyMonad a) = a

asAMonad :: (forall m. (Monad m) => (a -> m b) -> a -> m b) -> (a -> b) -> a -> b
asAMonad f a = fromEmptyMonad . f (EmptyMonad . a)

treeIfyBinOp :: Formula -> Formula
treeIfyBinOp (BinOp _ []) = error "Impossible to treeIfy"
treeIfyBinOp f@(BinOp _ [_,_]) = f
treeIfyBinOp (BinOp op (f1:f2:fs)) = innerNode $ assocOfBinOp op
        where innerNode OpAssocLeft = BinOp op $ (BinOp op [f1, f2]) : fs
              innerNode OpAssocRight = BinOp op [f1, BinOp op $ f2 : fs]
treeIfyBinOp _ = error "treeIfy of non binop formula"

biAssoc :: (a -> a -> Either a (a,a)) -> [a] -> [a]
biAssoc _ [] = []
biAssoc _ [x] = [x]
biAssoc f [x,y] = case f x y of
    Left v -> [v]
    Right (v1, v2) -> [v1, v2]
biAssoc f (x:y:xs) = case f x y of
    Left v -> biAssoc f (v:xs)
    Right (v1, v2) -> v1 : biAssoc f (v2:xs)

biAssocM :: (Monad m) => (a -> a -> m (Either a (a,a))) -> [a] -> m [a]
biAssocM f lst = assocInner lst
    where assocInner [] = return []
          assocInner [x] = return [x]
          assocInner [x,y] = f x y >>= \val -> case val of
              Left v -> return [v]
              Right (v1, v2) -> return [v1, v2]
          assocInner (x:y:xs) = f x y >>= \val -> case val of
              Left v -> assocInner (v:xs)
              Right (v1, v2) -> assocInner (v2:xs) >>= return . (v1:) 

