{-# LANGUAGE Rank2Types #-}
-- | Utility function/types used in the project.
module EqManips.Algorithm.Utils ( EmptyMonad( .. )
                                , biAssocM 
                                , biAssoc
                                , asAMonad
                                , fromEmptyMonad 
                                , treeIfyBinOp 
                                , listifyBinOp 
                                , parseFormula
                                ) where
import Text.Parsec.Error( ParseError )
import Text.ParserCombinators.Parsec.Prim( runParser )
import Control.Applicative
import EqManips.Propreties
import EqManips.Types
import EqManips.FormulaIterator
import EqManips.Linker

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
asAMonad :: (forall m. (Monad m) => (a -> m b) -> a -> m b) -> (a -> b) -> a -> b
asAMonad f a = fromEmptyMonad . f (EmptyMonad . a)


-----------------------------------------------------------
--          Parsing formula
-----------------------------------------------------------
-- | Helper function to parse a formula and apply all
-- needed algorithm to be able to apply them
parseFormula :: String -> Either ParseError Formula
parseFormula text = rez
    where parsed = runParser expr () "FromFile" text
          rez = case parsed of
             Left _ -> parsed
             Right f -> Right . listifyFormula $ linkFormula f

-- | listify a whole formula
listifyFormula :: Formula -> Formula
listifyFormula = depthFirstFormula `asAMonad` listifyBinOp 

-- | Given a binary operator in binary tree form,
-- transform it in list form.
listifyBinOp :: Formula -> Formula
listifyBinOp f@(BinOp op _) = BinOp op $ translate f
    where translate = flatten (op `obtainProp` AssocSide)
          flatten OpAssocRight = rightLister
          flatten OpAssocLeft 
                | op `hasProp` Associativ = rightLister . BinOp op . leftLister []
                | otherwise = leftLister []

          -- left associative operator packing.
          leftLister acc (BinOp op' (f1@(BinOp op'' _): f2))
                | op' == op'' = leftLister (f2 ++ acc) f1
          leftLister acc (BinOp op' fl)
                | op' == op = fl ++ acc
          leftLister acc final = final : acc

          -- right associative operator packing.
          rightLister (BinOp op' [f1, f2@(BinOp op'' _)])
                | op' == op'' = f1 : flatten OpAssocRight f2 
          rightLister (BinOp op' fl)
                | op' == op = fl
          rightLister final = [final]

listifyBinOp a = a

-- | Given a formula where all binops are in list
-- forms, transform it back to binary tree.
treeIfyBinOp :: Formula -> Formula
treeIfyBinOp (BinOp _ []) = error "Impossible to treeIfy"
treeIfyBinOp f@(BinOp _ [_,_]) = f
treeIfyBinOp (BinOp op (f1:f2:fs)) = innerNode $ op `obtainProp` AssocSide
        where innerNode OpAssocLeft = BinOp op $ (BinOp op [f1, f2]) : fs
              innerNode OpAssocRight = BinOp op [f1, BinOp op $ f2 : fs]
treeIfyBinOp _ = error "treeIfy of non binop formula"

biAssoc :: (a -> a -> Either a (a,a)) 
        -> (a -> a -> Either a (a,a)) 
        -> [a] -> [a]
biAssoc f finv lst = fromEmptyMonad 
                   $ biAssocM (\a -> return . f a) 
                              (\a -> return . finv a)
                              lst

biAssocM :: (Monad m) 
         => (a -> a -> m (Either a (a,a))) 
         -> (a -> a -> m (Either a (a,a))) 
         -> [a] -> m [a]
biAssocM f finv lst = assocInner f lst
    where assocInner _ [] = return []
          assocInner _ [x] = return [x]
          assocInner f' [x,y] = f' x y >>= \val -> case val of
              Left v -> return [v]
              Right (v1, v2) -> return [v1, v2]
          assocInner f' (x:y:xs) = f' x y >>= \val -> case val of
              Left v -> assocInner f' (v:xs)
              Right (v1, v2) -> assocInner finv (v2:xs) >>= return . (v1:) 

