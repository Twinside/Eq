module Language.Eq.Algorithm.Eval.Ratio( ratioEvalRules ) where

{-import qualified Language.Eq.ErrorMessages as Err-}
import Language.Eq.Types
import Language.Eq.Algorithm.Eval.Utils
import Language.Eq.Algorithm.Eval.Types

-- The two following rules can generate 0 in the polynomial
-- we have to clean them
-----------------------------------------------
----            '+'
-----------------------------------------------
add :: EvalOp
add (Fraction r1) (Fraction r2) = left . Fraction $ r1 + r2
add (CInteger i) (Fraction r) = left . Fraction $ toRational i + r
add (Fraction r) (CInteger i) = left . Fraction $ r + toRational i
add a b = right (a,b)

-----------------------------------------------
----            '-'
-----------------------------------------------
sub :: EvalOp
sub (Fraction r1) (Fraction r2) = left . Fraction $ r1 - r2
sub (CInteger i) (Fraction r) = left . Fraction $ toRational i - r
sub (Fraction r) (CInteger i) = left . Fraction $ r - toRational i
sub a b = right (a,b)

-----------------------------------------------
----            '*'
-----------------------------------------------
mul :: EvalOp
mul (Fraction r1) (Fraction r2) = left . Fraction $ r1 * r2
mul (CInteger i) (Fraction r) = left . Fraction $ toRational i * r
mul (Fraction r) (CInteger i) = left . Fraction $ r * toRational i
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
ratioEvalRules :: EvalFun
ratioEvalRules (BinOp _ OpAdd fs) = binEval OpAdd add add fs
ratioEvalRules (BinOp _ OpSub fs) = binEval OpSub sub add fs
ratioEvalRules (BinOp _ OpMul fs) = binEval OpMul mul mul fs
ratioEvalRules (BinOp _ OpDiv fs) = binEval OpDiv division mul fs
ratioEvalRules end = return end

