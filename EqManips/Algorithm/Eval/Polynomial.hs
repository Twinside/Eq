module EqManips.Algorithm.Eval.Polynomial( polyEvalRules ) where

import qualified EqManips.ErrorMessages as Err
import EqManips.Types
import EqManips.Polynome
import EqManips.EvaluationContext
import EqManips.Algorithm.Cleanup
import EqManips.Algorithm.Utils
import EqManips.Algorithm.Eval.Utils
import EqManips.Algorithm.Eval.Types

leftclean :: FormulaPrim -> EqContext (Either FormulaPrim a)
leftclean = left . unTagFormula . cleanup . Formula 

-- The two following rules can generate 0 in the polynomial
-- we have to clean them
-----------------------------------------------
----            '+'
-----------------------------------------------
add :: EvalOp
add (Poly p1) (Poly p2) = leftclean . Poly $ p1 + p2
add v1 (Poly p) | isFormulaScalar v1 = leftclean . Poly $ (PolyRest $ scalarToCoeff v1) + p
add (Poly p) v2 | isFormulaScalar v2 = leftclean . Poly $ p + (PolyRest $ scalarToCoeff v2)
add (Variable v) (Poly p) = leftclean . Poly $ Polynome v [(CoeffInt 1, PolyRest $ CoeffInt 1)] + p
add (Poly p) (Variable v) = left . Poly $ p + Polynome v [(CoeffInt 1, PolyRest $ CoeffInt 1)]

add (BinOp OpPow [Variable v, degree]) (Poly p) 
    | isFormulaScalar degree = leftclean . Poly $ Polynome v [(scalarToCoeff degree, PolyRest $ CoeffInt 1)] + p
add (Poly p) (BinOp OpPow [Variable v, degree]) 
    | isFormulaScalar degree = leftclean . Poly $ p + Polynome v [(scalarToCoeff degree, PolyRest $ CoeffInt 1)]
add e e' = right (e, e')

-----------------------------------------------
----            '-'
-----------------------------------------------
sub :: EvalOp
sub v1 (Poly p) | isFormulaScalar v1 = leftclean . Poly $ (PolyRest $ scalarToCoeff v1) - p
sub (Poly p) v2 | isFormulaScalar v2 = leftclean . Poly $ p - (PolyRest $ scalarToCoeff v2)
sub (Variable v) (Poly p) = leftclean . Poly $ Polynome v [(CoeffInt 1, PolyRest $ CoeffInt 1)] - p
sub (Poly p) (Variable v) = leftclean . Poly $ p - Polynome v [(CoeffInt 1, PolyRest $ CoeffInt 1)]
sub (BinOp OpPow [Variable v, degree]) (Poly p) 
    | isFormulaScalar degree = leftclean . Poly $ Polynome v [(scalarToCoeff degree, PolyRest $ CoeffInt 1)] - p
sub (Poly p) (BinOp OpPow [Variable v, degree]) 
    | isFormulaScalar degree = leftclean . Poly $ p - Polynome v [(scalarToCoeff degree, PolyRest $ CoeffInt 1)]
sub e e' = right (e,e')

-----------------------------------------------
----            '*'
-----------------------------------------------
mul :: EvalOp
mul (Poly p1) (Poly p2) = left . Poly $ p1 * p2
mul v1 (Poly p) | isFormulaScalar v1 = left . Poly $ polyCoeffMap (scalarToCoeff v1 *) p
mul (Poly p) v2 | isFormulaScalar v2 = left . Poly $ polyCoeffMap (* scalarToCoeff v2) p
mul (Variable v) (Poly p) = left . Poly $ Polynome v [(CoeffInt 1, PolyRest $ CoeffInt 1)] * p
mul (Poly p) (Variable v) = left . Poly $ p * Polynome v [(CoeffInt 1, PolyRest $ CoeffInt 1)]
mul (BinOp OpPow [Variable v, degree]) (Poly p) 
    | isFormulaScalar degree = left . Poly $ Polynome v [(scalarToCoeff degree, PolyRest $ CoeffInt 1)] * p
mul (Poly p) (BinOp OpPow [Variable v, degree]) 
    | isFormulaScalar degree = left . Poly $ p * Polynome v [(scalarToCoeff degree, PolyRest $ CoeffInt 1)]
mul e e' = right (e, e')

-----------------------------------------------
----        '/'
-----------------------------------------------
-- | Handle the division operator. Nicely handle the case
-- of division by 0.
division :: EvalOp
division v1 (Poly p) | isFormulaScalar v1 = left . Poly $ polyCoeffMap (scalarToCoeff v1 /) p
division (Poly p) v2 | isFormulaScalar v2 = left . Poly $ polyCoeffMap (/ scalarToCoeff v2) p
division (Poly p) (Poly p2) = case syntheticDiv p p2 of
        (Nothing, Nothing) -> right (Poly p, Poly p2)
        (Nothing, Just _) -> right (Poly p, Poly p2)
        (Just quotient, Nothing) -> left $ Poly quotient
        (Just quotient, Just rest) -> left $ Poly quotient + (Poly rest / Poly p2)
division f1 f2 = right (f1, f2)

-- | If a polynome's variable is bound, replace it by the real
-- the value.
substitutePolynome :: EvalFun -> Polynome -> Formula ListForm -> EqContext FormulaPrim
substitutePolynome _ (PolyRest _) _ = error Err.polynome_no_coeff_substitution 
substitutePolynome evaluator (Polynome _var coefs) (Formula subst) =
    evaluator $ BinOp OpAdd added
        where added = [formulize subPoly * (subst ** coefToFormula degree) | (degree, subPoly) <- coefs]
              formulize (PolyRest coeff) = coefToFormula coeff
              formulize normalPolynome = Poly normalPolynome

-----------------------------------------------
----        General evaluation
-----------------------------------------------
-- | General evaluation/reduction function
polyEvalRules :: EvalFun -> EvalFun
polyEvalRules _ (BinOp OpAdd fs) = binEval OpAdd add add fs
polyEvalRules _ (BinOp OpSub fs) = binEval OpSub sub add fs
polyEvalRules _ (BinOp OpMul fs) = binEval OpMul mul mul fs
polyEvalRules _ (BinOp OpDiv fs) = binEval OpDiv division mul fs
polyEvalRules evaluator p@(Poly pol@(Polynome var _)) =
    symbolLookup var >>= maybe (return p) (substitutePolynome evaluator pol)

polyEvalRules _ end = return end

