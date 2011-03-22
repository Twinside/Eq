module EqManips.Algorithm.Simplify( simplifyFormula ) where

import Control.Applicative

import EqManips.Types
import EqManips.EvaluationContext
import EqManips.Algorithm.Eval.Utils
import EqManips.Algorithm.Eval.Types

#ifdef _DEBUG
import EqManips.Algorithm.Utils

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
addSimplification eval a (BinOp _ OpMul [b, c])
    | hashOfFormula a == hashOfFormula c 
        && a == c = do
#ifdef _DEBUG
        tracer "Triggered '+' simplification" OpAdd a (BinOp 0 OpMul [b, c])
#endif
        subCoeff <- eval $ b + 1
        left $ subCoeff * c

addSimplification eval (BinOp _ OpMul [a, c]) b
    | hashOfFormula c == hashOfFormula b 
        && b == c = do
#ifdef _DEBUG
        tracer "Triggered '+' simplification" OpAdd (BinOp 0 OpMul [a,c]) b
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
subSimplification eval first@(BinOp _ OpMul [a, c]) b
    | hashOfFormula c == hashOfFormula b 
        && b == c = do
#ifdef _DEBUG
        tracer "Triggered '-' simplification" OpSub (BinOp 0 OpMul [a, c]) b
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
simplifyFormula _ formu = pure formu

