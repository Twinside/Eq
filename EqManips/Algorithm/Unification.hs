{-# OPTIONS_GHC -fno-warn-orphans #-}
module EqManips.Algorithm.Unification( unify, getFirstUnifying ) where

import Control.Applicative
import EqManips.Types
import Control.Monad.State.Lazy
import Data.List( foldl' )

infix 4 =~=

instance Applicative (State s) where
    pure = return 
    a <*> b = 
        do { a' <- a; b' <- b; return $ a' b' }
    
type UnificationContext a = State [(String, FormulaPrim)] a

-- | Just a little shortcut to be able to write more
-- consise code.
(=~=) :: FormulaPrim -> FormulaPrim
      -> UnificationContext Bool
a =~= b = unifyFormula a b

-- | Return the first pattern matching the given formula
-- and a list of substitution to be made on the function
-- body.
getFirstUnifying :: [([FormulaPrim], FormulaPrim)]
                 -> [FormulaPrim]
                 -> Maybe (FormulaPrim, [(String,FormulaPrim)])
getFirstUnifying matches toMatch = foldl' unif Nothing matches
    where unif Nothing (args, body) =
              let (rez, list) = runState (unifyList args toMatch) []
              in if rez then Just (body, list)
                        else Nothing
          unif j@(Just _) _ = j
          
-- | Try to Unify two formula, return a list of substitution
-- to transform a into b in case of success.
unify :: Formula anyKind -> Formula anyKind
      -> Maybe [(String, Formula TreeForm)]
unify (Formula a) (Formula b) =
     if rez
        then Nothing
        else Just $ [(s, Formula f) | (s,f) <- list]
    where (rez, list) = runState (a =~= b) []

-- | Helper function to unify list of formula side by side.
-- Used for "tuples"/arguments
unifyList :: [FormulaPrim] -> [FormulaPrim] -> UnificationContext Bool
unifyList l1 l2 
	| length l1 == length l2 =
		let valid acc (a,b) = (acc &&) <$> (a =~= b)
		in foldM valid True $ zip l1 l2
	| otherwise = return False

-- | Real function that implement unification.
-- origin pattern (function args...), to unify
unifyFormula :: FormulaPrim -- ^ Pattern
             -> FormulaPrim -- ^ to apply
             -> UnificationContext Bool
unifyFormula (App f1 l1) (App f2 l2) =
    (&&) . valid <$> (f1 =~= f2) <*> unifyList l1 l2
        where valid = (&&) $ length l1 == length l2 

unifyFormula (Truth a) (Truth b) =
    return $ a == b

unifyFormula (CInteger i1) (CInteger i2) =
    return $ i1 == i2

unifyFormula (CFloat i1) (CFloat i2) =
    return $ i1 == i2

unifyFormula (NumEntity e1) (NumEntity e2) =
    return $ e1 == e2

unifyFormula (BinOp op1 l1) (BinOp op2 l2)
    | op1 == op2 && length l1 == length l2 = unifyList l1 l2
    | otherwise = return False

unifyFormula (UnOp op1 f1) (UnOp op2 f2) =
    ((&&) $ op1 == op2) <$> (f1 =~= f2)

unifyFormula (Variable v1) f2 = do
    symbolList <- get
    maybe (do put $ (v1,f2) : symbolList
              return True)
          (return . (f2 ==))
          $ lookup v1 symbolList

unifyFormula _ _ = return False

