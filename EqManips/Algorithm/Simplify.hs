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
addSimplification eval a second@(BinOp _ OpMul [b, c])
    | hashOfFormula a == hashOfFormula c 
        && a == c = do
#ifdef _DEBUG
        tracer "Triggered '+' simplification" OpAdd a second
#endif
        subCoeff <- eval $ b + 1
        left $ subCoeff * c

addSimplification eval first@(BinOp _ OpMul [a, c]) b
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
subSimplification eval first@(BinOp _ OpMul [a, c]) b
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

mulSimplification :: EvalFun -> EvalOp
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

