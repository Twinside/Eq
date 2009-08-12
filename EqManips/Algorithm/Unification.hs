{-# OPTIONS_GHC -fno-warn-orphans #-}
module EqManips.Algorithm.Unification( unify, getFirstUnifying ) where

import Control.Applicative
import EqManips.Types
import Control.Monad.State.Lazy

infix 4 =~=

instance Applicative (State s) where
    pure = return 
    a <*> b = 
        do { a' <- a; b' <- b; return $ a' b' }
    
type UnificationContext a = State [(String, Formula)] a

(=~=) :: Formula -> Formula -> UnificationContext Bool
a =~= b = unifyFormula a b

getFirstUnifying :: [([Formula], Formula)] -> [Formula] -> Maybe (Formula,[(String,Formula)])
getFirstUnifying matches toMatch = unif matches
    where unif [] = Nothing
          unif ((args, body):xs) =
              let (rez, list) = runState (unifyList args toMatch) []
              in if rez then Just (body, list)
                        else unif xs
          

unify :: Formula -> Formula -> Maybe [(String, Formula)]
unify a b = if rez then Nothing else Just list
    where (rez, list) = runState (a =~= b) []

unifyList :: [Formula] -> [Formula] -> UnificationContext Bool
unifyList l1 l2 = foldM valid True $ zip l1 l2
    where valid acc (a,b) = (acc &&) <$> (a =~= b)

-- | origin pattern (function args...), to unify
unifyFormula :: Formula -> Formula -> UnificationContext Bool
unifyFormula (App f1 l1) (App f2 l2) =
    (&&) . valid <$> (f1 =~= f2) <*> unifyList l1 l2
        where valid = (&&) $ length l1 == length l2 

unifyFormula (CInteger i1) (CInteger i2) =
    return $ i1 == i2

unifyFormula (CFloat i1) (CFloat i2) =
    return $ i1 == i2

unifyFormula (NumEntity e1) (NumEntity e2) =
    return $ e1 == e2

unifyFormula (UnOp op1 f1) (UnOp op2 f2) =
    ((&&) $ op1 == op2) <$> (f1 =~= f2)

unifyFormula (Variable v1) f2 = do
    symbolList <- get
    maybe (do put $ (v1,f2) : symbolList
              return True)
          (return . (f2 ==))
          $ lookup v1 symbolList

unifyFormula _ _ = return False

