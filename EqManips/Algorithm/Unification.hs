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

unifyFormula (Fraction f1) (Fraction f2) =
    return $ f1 == f2

unifyFormula (Complex (re, im)) (Complex (re2, im2)) =
    (&&) <$> (re =~= re2) <*> (im =~= im2)

unifyFormula (Poly left@(Polynome _ _))
             (Poly right@(Polynome _ _)) =
  if valid then and <$> mapM (uncurry checkSymbol) subs
           else return False
    where (subs, valid) = subPolyEq left right
          subPolyEq (PolyRest a) (PolyRest b)   = ([], a == b)
          subPolyEq (PolyRest _) (Polynome _ _) = ([], False)
          subPolyEq (Polynome _ _) (PolyRest _) = ([], False)
          -- Equals at an alpha renaming
          subPolyEq (Polynome var1 lst1')
                    replacement@(Polynome _ lst2') =
                if valid' then ((var1, Poly replacement) : substitu, True)
                          else ([], False)
            where (substitu, valid') = verifyCoeff lst1' lst2'

          verifyCoeff a b = foldr coefEq ([], True) $ zip a b

          coefEq ((c1,sub1),(c2,sub2)) (alphas, acc) =
              (alphas ++ alpha', acc && c1 == c2 && acc')
                  where (alpha', acc') = subPolyEq sub1 sub2

unifyFormula (BinOp OpAdd _added) (Poly (Polynome _v _lst)) =
    return False

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
    (op1 == op2 &&) <$> (f1 =~= f2)

unifyFormula (Variable v1) f2 = checkSymbol v1 f2

unifyFormula _ _ = return False

checkSymbol :: String -> FormulaPrim -> UnificationContext Bool
checkSymbol var what = do
    symbolList <- get
    maybe (do put $ (var, what) : symbolList
              return True)
          (return . (what ==))
          $ lookup var symbolList

