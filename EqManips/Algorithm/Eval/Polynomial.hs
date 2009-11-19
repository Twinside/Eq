module EqManips.Algorithm.Eval.Polynomial( polyEvalRules ) where

import EqManips.Types
import EqManips.Polynome
import EqManips.Algorithm.Utils
import EqManips.Algorithm.Eval.Utils
import EqManips.Algorithm.Eval.Types

-----------------------------------------------
----            '+'
-----------------------------------------------
add :: EvalOp
add (Poly p1) (Poly p2) = left . Poly $ p1 + p2
add v1 (Poly p) | isFormulaScalar v1 = left . Poly $ polyCoeffMap (\a -> (scalarToCoeff v1) + a) p
add (Poly p) v2 | isFormulaScalar v2 = left . Poly $ polyCoeffMap (+ (scalarToCoeff v2)) p
add e e' = right (e, e')

-----------------------------------------------
----            '-'
-----------------------------------------------
sub :: EvalOp
sub v1 (Poly p) | isFormulaScalar v1 = left . Poly $ polyCoeffMap (\a -> (scalarToCoeff v1) - a) p
sub (Poly p) v2 | isFormulaScalar v2 = left . Poly $ polyCoeffMap (\a -> a - (scalarToCoeff v2)) p
sub e e' = right (e,e')

-----------------------------------------------
----            '*'
-----------------------------------------------
mul :: EvalOp
mul (CInteger i1) (CInteger i2) = left . CInteger $ i1 * i2
mul (Poly p1) (Poly p2) = left . Poly $ p1 * p2
mul v1 (Poly p) | isFormulaScalar v1 = left . Poly $ polyCoeffMap (\a -> (scalarToCoeff v1) * a) p
mul (Poly p) v2 | isFormulaScalar v2 = left . Poly $ polyCoeffMap (* (scalarToCoeff v2)) p
mul e e' = right (e, e')

-----------------------------------------------
----        '/'
-----------------------------------------------
-- | Handle the division operator. Nicely handle the case
-- of division by 0.
division :: EvalOp
division v1 (Poly p) | isFormulaScalar v1 = left . Poly $ polyCoeffMap (\a -> (scalarToCoeff v1) / a) p
division (Poly p) v2 | isFormulaScalar v2 = left . Poly $ polyCoeffMap (/ (scalarToCoeff v2)) p
division f1 f2 = right (f1, f2)

-----------------------------------------------
----        General evaluation
-----------------------------------------------
-- | General evaluation/reduction function
polyEvalRules :: EvalFun
polyEvalRules (BinOp OpAdd fs) = binEval OpAdd add add fs
polyEvalRules (BinOp OpSub fs) = binEval OpSub sub add fs
polyEvalRules (BinOp OpMul fs) = binEval OpMul mul mul fs
polyEvalRules (BinOp OpDiv fs) = binEval OpDiv division mul fs
polyEvalRules end = return end

