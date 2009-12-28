module EqManips.Algorithm.Eval.Complex( complexEvalRules ) where

{-import qualified EqManips.ErrorMessages as Err-}
import Control.Applicative( (<$>), (<*>) )
import EqManips.Types
import EqManips.Algorithm.Utils
import EqManips.Algorithm.Eval.Utils
import EqManips.Algorithm.Eval.Types

reshape :: FormulaPrim -> FormulaPrim
reshape = unTagFormula . listifyFormula . Formula

-- The two following rules can generate 0 in the polynomial
-- we have to clean them
-----------------------------------------------
----            '+'
-----------------------------------------------
add :: EvalFun -> EvalOp
add eval (Complex (r1,i1)) (Complex (r2, i2)) =
    (\real imag -> Left $ Complex (real, imag))
        <$> eval (reshape $ r1 + r2)
        <*> eval (reshape $ i1 + i2)
add eval (Complex (r1,i1)) rightp | isFormulaScalar rightp =
    (\real -> Left $ Complex (real, i1)) <$> eval (reshape $ r1 + rightp)
add eval leftp (Complex (r1,i1)) | isFormulaScalar leftp =
    (\real -> Left $ Complex (real, i1)) <$> eval (reshape $ leftp + r1)
add _ a b = right (a, b)

-----------------------------------------------
----            '-'
-----------------------------------------------
sub :: EvalFun -> EvalOp
sub eval (Complex (r1,i1)) (Complex (r2, i2)) =
    (\real imag -> Left $ Complex (real, imag))
        <$> eval (reshape $ r1 - r2)
        <*> eval (reshape $ i1 - i2)
sub eval (Complex (r1,i1)) rightp | isFormulaScalar rightp =
    (\real -> Left $ Complex (real, i1)) <$> eval (reshape $ r1 - rightp)
sub eval leftp (Complex (r1,i1)) | isFormulaScalar leftp =
    (\real -> Left $ Complex (real, i1)) <$> eval (reshape $ leftp - r1)
sub _ a b = right (a, b)

-----------------------------------------------
----            '*'
-----------------------------------------------
mul :: EvalFun -> EvalOp
-- (a + ib)(a' + ib') = a*a' - b*b' + a'*ib + a*ib'
mul eval (Complex (r1,i1)) (Complex (r2, i2)) =
    (\real imag -> Left $ Complex (real, imag))
        <$> eval (reshape $ r1 * r2 - i1 * i2)
        <*> eval (reshape $ r2 * i1 + r1 * i2)
mul eval (Complex (r1,i1)) rightp | isFormulaScalar rightp =
    (\real imag -> Left $ Complex (real, imag))
            <$> eval (reshape $ r1 * rightp)
            <*> eval (reshape $ i1 * rightp)
mul eval leftp (Complex (r1,i1)) | isFormulaScalar leftp =
    (\real imag -> Left $ Complex (real, imag))
            <$> eval (reshape $ leftp * r1)
            <*> eval (reshape $ leftp * i1)
mul _ a b = right (a,b)

-----------------------------------------------
----        '/'
-----------------------------------------------
-- | Handle the division operator. Nicely handle the case
-- of division by 0.
division :: EvalFun -> EvalOp
division eval (Complex (a,b)) (Complex (c, d)) =
    (\real imag -> Left $ Complex (real, imag))
        <$> eval (reshape $ realNumerator / denom)
        <*> eval (reshape $ imagNumerator / denom)
    where realNumerator = a * c + b * d
          imagNumerator = b * c - a * d
          denom = c ** CInteger 2 + d ** CInteger 2
division eval (Complex (r1,i1)) rightp | isFormulaScalar rightp =
    (\real imag -> Left $ Complex (real, imag))
            <$> eval (reshape $ r1 / rightp)
            <*> eval (reshape $ i1 / rightp)
division eval leftp (Complex (r1,i1)) | isFormulaScalar leftp =
    (\real imag -> Left $ Complex (real, imag))
            <$> eval (reshape $ leftp / r1)
            <*> eval (reshape $ leftp / i1)
division _ a b = right (a,b)

-----------------------------------------------
----        General evaluation
-----------------------------------------------
-- | General evaluation/reduction function
complexEvalRules :: EvalFun -> EvalFun
complexEvalRules f (BinOp OpAdd fs) = binEval OpAdd (add f) (add f) fs
complexEvalRules f (BinOp OpSub fs) = binEval OpSub (sub f) (add f) fs
complexEvalRules f (BinOp OpMul fs) = binEval OpMul (mul f) (mul f) fs
complexEvalRules f (BinOp OpDiv fs) = binEval OpDiv (division f) (mul f) fs
complexEvalRules _ end = return end

