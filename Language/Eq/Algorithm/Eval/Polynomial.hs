module Language.Eq.Algorithm.Eval.Polynomial( polyEvalRules ) where

import Data.Either( partitionEithers )

import qualified Language.Eq.ErrorMessages as Err
import Language.Eq.Types
import Language.Eq.Polynome
import Language.Eq.EvaluationContext
import Language.Eq.Algorithm.Cleanup
import Language.Eq.Algorithm.Utils
import Language.Eq.Algorithm.Eval.Utils
import Language.Eq.Algorithm.Eval.Types

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
#ifdef _DEBUG
sub leftArg@(Poly _ p1) rightArg@(Poly _ p2) = 
  addTrace ( "Polynome/Polynome '-'"
           , treeIfyFormula . Formula 
                            $ leftArg - rightArg) >>
#else
sub (Poly _ p1) (Poly _ p2) = 
#endif
    leftclean (poly $ p1 - p2)

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
division p1@(Poly _ p) p2f@(Poly _ p2) = 
    let unconstruct = unTagFormula  . cleanupRules . Formula . polyAsFormula
    in case syntheticDiv p p2 of
        (Nothing, Nothing) -> right (p1, p2f)
        (Nothing, Just _) -> right (p1, p2f)
        (Just quotient, Nothing) -> left $ unconstruct quotient
        (Just quotient, Just rest) -> left $ unconstruct quotient
                                           + ( unconstruct rest 
                                             / unconstruct p2)
division f1 f2 = right (f1, f2)

-- | If a polynome's variable is bound, replace it by the real
-- the value.
substitutePolynome :: EvalFun -> Polynome -> Formula ListForm -> EqContext FormulaPrim
substitutePolynome _ (PolyRest _) _ = error Err.polynome_no_coeff_substitution 
substitutePolynome evaluator (Polynome _var coefs) (Formula subst) =
    evaluator $ binopize added
        where added = [if degree /= 1
                          then formulize subPoly * (subst ** coefToFormula degree)
                          else formulize subPoly * subst | (degree, subPoly) <- coefs]
              formulize (PolyRest coeff) = coefToFormula coeff
              formulize normalPolynome = poly normalPolynome

              binopize [a] = a
              binopize a = binOp OpAdd a

checkPolynomeBinding :: EvalFun -> Polynome -> EqContext (Either Polynome FormulaPrim)
checkPolynomeBinding _           p@(PolyRest _) = return $ Left p
checkPolynomeBinding evaluator pol@(Polynome var coefList) = do
    varBound <- symbolLookup var
    case varBound of
         Just bound ->
             substitutePolynome evaluator pol bound >>= (return . Right)
         Nothing -> do
            subs <- mapM (\(coeff,p) -> do
                subPoly <- checkPolynomeBinding evaluator p
                case subPoly of
                     Left filteredPoly -> return . Left $ (coeff, filteredPoly)
                     Right formu -> return . Right $
                         formu * poly (Polynome var [( coeff
                                                     , PolyRest $ CoeffInt 1)])
                ) coefList
            case  partitionEithers subs of
                ([], []) -> error "Impossible case"
                ([], formulas) ->
                    return . Right $ binOp OpAdd formulas
                (polys, []) ->
                    return . Left $ Polynome var polys
                (polys, formulas) ->
                    return . Right .  binOp OpAdd
                        $ poly (Polynome var polys) : formulas
                        

-----------------------------------------------
----        General evaluation
-----------------------------------------------
-- | General evaluation/reduction function
polyEvalRules :: EvalFun -> EvalFun
polyEvalRules _ (BinOp _ OpAdd fs) = binEval OpAdd add add fs
polyEvalRules _ (BinOp _ OpSub fs) = binEval OpSub sub add fs
polyEvalRules _ (BinOp _ OpMul fs) = binEval OpMul mul mul fs
polyEvalRules _ (BinOp _ OpDiv fs) = binEval OpDiv division mul fs
polyEvalRules evaluator (Poly _ pol@(Polynome _ _)) = do
    checkPolynomeBinding evaluator pol 
    >>= either (return . poly) return
polyEvalRules _ end = return end

