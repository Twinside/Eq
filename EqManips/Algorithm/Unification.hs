{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EqManips.Algorithm.Unification( unify, getFirstUnifying ) where

import Data.List( foldl' )

import Control.Applicative
import Data.Monoid
import Control.Monad.Writer
import Control.Monad.State.Lazy

import EqManips.Types
import EqManips.Polynome
import EqManips.Algorithm.Utils

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
(=~=) = unifyFormula

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
        else Just [(s, Formula f) | (s,f) <- list]
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
                 if valid 
                  then and <$> mapM (uncurry checkSymbol) subs
                  else pure valid
    where (valid, subs :: [(String, FormulaPrim)]) = runWriter $ subPolyEq left right
          -- n == n'
          subPolyEq (PolyRest a) (PolyRest b)   = return $ a == b
          -- n == x^y + ... + ... <=> False
          subPolyEq (PolyRest _) (Polynome _ _) = return False
          -- x^y + ... + ... == n <=> False
          subPolyEq (Polynome _ _) (PolyRest _) = return False

          -- 1 * x ^ 1 <=> var / poly equivalence
          subPolyEq (Polynome var1 [(c1, PolyRest c2)])
                    replacement@(Polynome _ lst2')
                | c1 == CoeffInt 1 && c2 == CoeffInt 1 =
                    tell [(var1, Poly replacement)] >> return True

          -- Are two polynoms equivalent?
          subPolyEq (Polynome var1 lst1')
                    replacement@(Polynome var2 lst2') = do
                        valid' <- verifyCoeff lst1' lst2'
                        when valid' $ tell [(var1, Variable var2)]
                        return valid'

          verifyCoeff a = foldM coefEq True . zip a

          coefEq acc ((c1,sub1),(c2,sub2)) =
              ((acc && c1 == c2) &&) <$> subPolyEq sub1 sub2

unifyFormula (BinOp OpAdd added) (Poly (Polynome v lst)) =
    {-
    if length added == length lst && valid
       then and <$> mapM (uncurry checkSymbol) adds
       else return valid
       -} return False
    
          {-
    where (valid, adds) = runWriter . validMatch . zip added $ zipper v lst
          zipper var = map (\(c, s) -> (v,c,s))

          -- a =~= x^y+z, ok it works
          validMatch ( Variable pvar, (var, c, sub)) =
              tell (pvar, Polynome var [(c,sub)]) >> return True

          -- a ^ b =~= 1 * x ^ y
          validMatch ( BinOp OpPow [ Variable pvar
                                   , Variable powvar]
                     , (var, c, PolyRest sub)) 
            | CoeffInt 1 == sub = do
                         tell (pvar, Variable var)
                         tell (powvar, coefToFormula c)
                         return True

          -- a ^ 15 =~= 1*x^15
          validMatch ( BinOp OpPow [ Variable pvar
                                   , CInteger i], (var, c, PolyRest sub))
            | CoeffInt 1 == sub && c == CoeffInt i =
                  tell [(pvar, Variable var)] >> return True

          -- n * ... <=> n' * x ^ 0
          -- else it's wrong
          validMatch ( BinOp OpMul (e:xs), (_, c, sub))
            | isFormulaScalar e = case sub of
                    PolyRest a -> return $ c == CoeffInt 0 && scalarToCoeff e == a
                    _          -> return False

          validMatch ( BinOp OpMul [Variable v], (var, c, PolyRest coeff))
            | c /= 0 = return False
            | otherwise = tell [(v, coefToFormula coeff)]
                       >> return True
          validMatch ( BinOp OpMul (Variable v:xs), (var1, c, poly@(Polynome _ _))) = do
              valid <- validMatch (BinOp OpMul xs, poly)
              when valid $ tell [(v, Variable var1)]
              return valid

          validMatch ( BinOp OpMul ((BinOp OpPow [ Variable pvar
                                                 , CInteger i ])
                                     :xs)
                     , (_, c,sub)) = return False

          -- General case : it's not valid.
          validMatch _ = return False
          -}

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

-- | Add symbol if it doesn't exists, and check for equality
-- of definition otherwise.
checkSymbol :: String -> FormulaPrim -> UnificationContext Bool
checkSymbol var what = do
    symbolList <- get
    maybe (do put $ (var, what) : symbolList
              return True)
          (return . (what ==))
          $ lookup var symbolList

