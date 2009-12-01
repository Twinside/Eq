module EqManips.Algorithm.Eval.Ratio( ratioEvalRules ) where

{-import qualified EqManips.ErrorMessages as Err-}
import EqManips.Types
import EqManips.Algorithm.Eval.Utils
import EqManips.Algorithm.Eval.Types

-- The two following rules can generate 0 in the polynomial
-- we have to clean them
-----------------------------------------------
----            '+'
-----------------------------------------------
add :: EvalOp
add (Fraction r1) (Fraction r2) = left . Fraction $ r1 + r2
add a b = right (a,b)

-----------------------------------------------
----            '-'
-----------------------------------------------
sub :: EvalOp
sub (Fraction r1) (Fraction r2) = left . Fraction $ r1 - r2
sub a b = right (a,b)

-----------------------------------------------
----            '*'
-----------------------------------------------
mul :: EvalOp
mul (Fraction r1) (Fraction r2) = left . Fraction $ r1 + r2
mul a b = right (a,b)

-----------------------------------------------
----        '/'
-----------------------------------------------
-- | Handle the division operator. Nicely handle the case
-- of division by 0.
division :: EvalOp
division (Fraction r1) (Fraction r2) = left . Fraction $ r1 / r2
division a b = right (a,b)

-----------------------------------------------
----        General evaluation
-----------------------------------------------
-- | General evaluation/reduction function
ratioEvalRules :: EvalFun -> EvalFun
ratioEvalRules _ (BinOp OpAdd fs) = binEval OpAdd add add fs
ratioEvalRules _ (BinOp OpSub fs) = binEval OpSub sub add fs
ratioEvalRules _ (BinOp OpMul fs) = binEval OpMul mul mul fs
ratioEvalRules _ (BinOp OpDiv fs) = binEval OpDiv division mul fs
ratioEvalRules _ end = return end

