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
addSimplification :: EvalOp
addSimplification a second@(BinOp _ OpMul [b, c])
    | hashOfFormula a == hashOfFormula c 
        && a == c = 
#ifdef _DEBUG
        tracer "Triggered '+' simplification" OpAdd a second >>
#endif
            left ((b + 1) * c)
addSimplification first@(BinOp _ OpMul [a, c]) b
    | hashOfFormula c == hashOfFormula b 
        && b == c = 
#ifdef _DEBUG
        tracer "Triggered '+' simplification" OpAdd first b >>
#endif
        left ((a + 1) * c)
addSimplification a b
    | hashOfFormula a == hashOfFormula b
        && a == b = 
#ifdef _DEBUG
        tracer "Triggered '+' simplification" OpAdd a b >>
#endif
        left (2 * a)
    | otherwise = right $ (a,b)

-- | '-' operator simplification
subSimplification :: EvalOp
subSimplification a b
    | hashOfFormula a == hashOfFormula b
        && a == b = 
#ifdef _DEBUG
        tracer "Triggered '-' simplification" OpSub a b >>
#endif
        left 0
    | otherwise = right (a,b)

mulSimplification :: EvalOp
mulSimplification a b
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
simplifyFormula :: FormulaPrim
                -> EqContext FormulaPrim
simplifyFormula (BinOp _ OpAdd lst) =
    binEval OpAdd addSimplification addSimplification lst
simplifyFormula (BinOp _ OpSub lst) =
    binEval OpAdd subSimplification addSimplification lst
simplifyFormula (BinOp _ OpMul lst) =
    binEval OpAdd mulSimplification mulSimplification lst
simplifyFormula f = pure f

