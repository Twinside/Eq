module Language.Eq.Algorithm.Simplify( simplifyFormula ) where

import Control.Applicative

import Language.Eq.Types
import Language.Eq.EvaluationContext
import Language.Eq.Algorithm.Eval.Utils
import Language.Eq.Algorithm.Eval.Types

#ifdef _DEBUG
import Language.Eq.Algorithm.Utils

tracer :: String -> BinOperator -> FormulaPrim -> FormulaPrim
       -> EqContext ()
tracer str op f1 f2 =
  addTrace (str, treeIfyFormula . Formula 
                                 $ binOp op [ f1, f2 ])
#endif

--------------------------------------------------
----            Operators
--------------------------------------------------

-- | '+' operator simplification.
-- Some propreties which should work for the addition
-- operation.
addSimplification :: EvalFun -> EvalOp
#ifdef _DEBUG
addSimplification eval a second@(BinOp _ OpMul [b, c])
#else
addSimplification eval a (BinOp _ OpMul [b, c])
#endif
    | hashOfFormula a == hashOfFormula c 
        && a == c = do
#ifdef _DEBUG
        tracer "Triggered '+' simplification" OpAdd a second
#endif
        subCoeff <- eval $ b + 1
        left $ subCoeff * c

#ifdef _DEBUG
addSimplification eval first@(BinOp _ OpMul [a, c]) b
#else
addSimplification eval (BinOp _ OpMul [a, c]) b
#endif
    | hashOfFormula c == hashOfFormula b 
        && b == c = do
#ifdef _DEBUG
        tracer "Triggered '+' simplification" OpAdd first b
#endif
        subCoeff <- eval $ a + 1
        left $ subCoeff * c
addSimplification _ a b
    | hashOfFormula a == hashOfFormula b
        && a == b = 
#ifdef _DEBUG
        tracer "Triggered '+' simplification" OpAdd a b >>
#endif
        left (2 * a)
    | otherwise = right $ (a,b)

-- | '-' operator simplification
subSimplification :: EvalFun -> EvalOp
{-subSimplification eval (Variable v) (BinOp _ OpDiv [a, somethingWithV])-}

{- if c == b  then a * c - b = (a-1) * c -}
#ifdef _DEBUG
subSimplification eval first@(BinOp _ OpMul [a, c]) b
#else
subSimplification eval (BinOp _ OpMul [a, c]) b
#endif
    | hashOfFormula c == hashOfFormula b 
        && b == c = do
#ifdef _DEBUG
        tracer "Triggered '-' simplification" OpSub first b
#endif
        subCoeff <- eval (a - 1)
        left (subCoeff * c)

subSimplification _ a b
    | hashOfFormula a == hashOfFormula b
        && a == b = 
#ifdef _DEBUG
        tracer "Triggered '-' simplification" OpSub a b >>
#endif
        left 0
    | otherwise = right (a,b)

--------------------------------------------------
----            '*' simplification
--------------------------------------------------
mulSimplification :: EvalFun -> EvalOp
mulSimplification eval (BinOp _ OpPow [a, c]) b
    | hashOfFormula a == hashOfFormula b
        && a == b = 
#ifdef _DEBUG
        tracer "Triggered '*' simplification" OpMul a b >>
#endif
        Left <$> eval (a ** (c + 1))

mulSimplification eval b (BinOp _ OpPow [a, c])
    | hashOfFormula a == hashOfFormula b
        && a == b = 
#ifdef _DEBUG
        tracer "Triggered '*' simplification" OpMul b a >>
#endif
        Left <$> eval (a ** (c + 1))

mulSimplification _ a b
    | hashOfFormula a == hashOfFormula b
        && a == b =
#ifdef _DEBUG
        tracer "Triggered '*' simplification" OpMul a b >>
#endif
        left (a ** 2)
    | otherwise = right (a,b)

divSimplification :: EvalFun -> EvalOp
divSimplification _ (BinOp _ OpMul lst) (CInteger constant)
    | any hasFraction lst = return . Left $ (binOp OpMul $ changeFraction lst)
        where hasFraction (Fraction _) = True
              hasFraction _ = False

              newCoeff frac = Fraction $ frac / toRational constant

              changeFraction [] = []
              changeFraction (Fraction f:xs) = newCoeff f : xs
              changeFraction (x:xs) = x : changeFraction xs

divSimplification _ a b = right (a,b)

--------------------------------------------------
----            Main Function
--------------------------------------------------
simplifyFormula :: EvalFun -> FormulaPrim
                -> EqContext FormulaPrim
simplifyFormula f (BinOp _ OpAdd lst) =
    binEval OpAdd (addSimplification f) (addSimplification f) lst
simplifyFormula f (BinOp _ OpSub lst) =
    binEval OpSub (subSimplification f) (addSimplification f) lst
simplifyFormula f (BinOp _ OpMul lst) =
    binEval OpMul (mulSimplification f) (mulSimplification f) lst
simplifyFormula f (BinOp _ OpDiv lst) =
    binEval OpDiv (divSimplification f) (mulSimplification f) lst
simplifyFormula _ formu = pure formu

