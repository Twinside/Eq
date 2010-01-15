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
add (Poly _ p1) (Poly _ p2) = leftclean . poly $ p1 + p2
add v1 (Poly _ p) | isFormulaScalar v1 = leftclean . poly $ (PolyRest $ scalarToCoeff v1) + p
add (Poly _ p) v2 | isFormulaScalar v2 = leftclean . poly $ p + (PolyRest $ scalarToCoeff v2)
add (Variable v) (Poly _ p) = leftclean . poly $ Polynome v [(CoeffInt 1, PolyRest $ CoeffInt 1)] + p
add (Poly _ p) (Variable v) = left . poly $ p + Polynome v [(CoeffInt 1, PolyRest $ CoeffInt 1)]

add (BinOp _ OpPow [Variable v, degree]) (Poly _ p) 
    | isFormulaScalar degree = leftclean . poly $ Polynome v [(scalarToCoeff degree, PolyRest $ CoeffInt 1)] + p
add (Poly _ p) (BinOp _ OpPow [Variable v, degree]) 
    | isFormulaScalar degree = leftclean . poly $ p + Polynome v [(scalarToCoeff degree, PolyRest $ CoeffInt 1)]
add e e' = right (e, e')

-----------------------------------------------
----            '-'
-----------------------------------------------
sub :: EvalOp
sub v1 (Poly _ p) | isFormulaScalar v1 = leftclean . poly $ (PolyRest $ scalarToCoeff v1) - p
sub (Poly _ p) v2 | isFormulaScalar v2 = leftclean . poly $ p - (PolyRest $ scalarToCoeff v2)
sub (Variable v) (Poly _ p) = leftclean . poly $ Polynome v [(CoeffInt 1, PolyRest $ CoeffInt 1)] - p
sub (Poly _ p) (Variable v) = leftclean . poly $ p - Polynome v [(CoeffInt 1, PolyRest $ CoeffInt 1)]
sub (BinOp _ OpPow [Variable v, degree]) (Poly _ p) 
    | isFormulaScalar degree = leftclean . poly $ Polynome v [(scalarToCoeff degree, PolyRest $ CoeffInt 1)] - p
sub (Poly _ p) (BinOp _ OpPow [Variable v, degree]) 
    | isFormulaScalar degree = leftclean . poly $ p - Polynome v [(scalarToCoeff degree, PolyRest $ CoeffInt 1)]
sub e e' = right (e,e')

-----------------------------------------------
----            '*'
-----------------------------------------------
mul :: EvalOp
mul (Poly _ p1) (Poly _ p2) = left . poly $ p1 * p2
mul v1 (Poly _ p) | isFormulaScalar v1 = left . poly $ polyCoeffMap (scalarToCoeff v1 *) p
mul (Poly _ p) v2 | isFormulaScalar v2 = left . poly $ polyCoeffMap (* scalarToCoeff v2) p
mul (Variable v) (Poly _ p) = left . poly $ Polynome v [(CoeffInt 1, PolyRest $ CoeffInt 1)] * p
mul (Poly _ p) (Variable v) = left . poly $ p * Polynome v [(CoeffInt 1, PolyRest $ CoeffInt 1)]
mul (BinOp _ OpPow [Variable v, degree]) (Poly _ p) 
    | isFormulaScalar degree = left . poly $ Polynome v [(scalarToCoeff degree, PolyRest $ CoeffInt 1)] * p
mul (Poly _ p) (BinOp _ OpPow [Variable v, degree]) 
    | isFormulaScalar degree = left . poly $ p * Polynome v [(scalarToCoeff degree, PolyRest $ CoeffInt 1)]
mul e e' = right (e, e')

-----------------------------------------------
----        '/'
-----------------------------------------------
-- | Handle the division operator. Nicely handle the case
-- of division by 0.
division :: EvalOp
division v1 (Poly _ p) | isFormulaScalar v1 = left . poly $ polyCoeffMap (scalarToCoeff v1 /) p
division (Poly _ p) v2 | isFormulaScalar v2 = left . poly $ polyCoeffMap (/ scalarToCoeff v2) p
division (Poly _ p) (Poly _ p2) = case syntheticDiv p p2 of
        (Nothing, Nothing) -> right (poly p, poly p2)
        (Nothing, Just _) -> right (poly p, poly p2)
        (Just quotient, Nothing) -> left $ poly quotient
        (Just quotient, Just rest) -> left $ poly quotient + (poly rest / poly p2)
division f1 f2 = right (f1, f2)

-- | If a polynome's variable is bound, replace it by the real
-- the value.
substitutePolynome :: EvalFun -> Polynome -> Formula ListForm -> EqContext FormulaPrim
substitutePolynome _ (PolyRest _) _ = error Err.polynome_no_coeff_substitution 
substitutePolynome evaluator (Polynome _var coefs) (Formula subst) =
    evaluator $ binOp OpAdd added
        where added = [formulize subPoly * (subst ** coefToFormula degree) | (degree, subPoly) <- coefs]
              formulize (PolyRest coeff) = coefToFormula coeff
              formulize normalPolynome = poly normalPolynome

-----------------------------------------------
----        General evaluation
-----------------------------------------------
-- | General evaluation/reduction function
polyEvalRules :: EvalFun -> EvalFun
polyEvalRules _ (BinOp _ OpAdd fs) = binEval OpAdd add add fs
polyEvalRules _ (BinOp _ OpSub fs) = binEval OpSub sub add fs
polyEvalRules _ (BinOp _ OpMul fs) = binEval OpMul mul mul fs
polyEvalRules _ (BinOp _ OpDiv fs) = binEval OpDiv division mul fs
polyEvalRules evaluator p@(Poly _ pol@(Polynome var _)) =
    symbolLookup var >>= maybe (return p) (substitutePolynome evaluator pol)

polyEvalRules _ end = return end

