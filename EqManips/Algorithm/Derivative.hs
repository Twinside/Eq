module EqManips.Algorithm.Derivative( derivateFormula
                                    , Var ) where

import Control.Applicative
import Control.Monad( foldM )
import Data.Monoid( Monoid( .. ), Any( .. ) )

import qualified EqManips.ErrorMessages as Err

import EqManips.Types
import EqManips.Polynome
import EqManips.EvaluationContext
import EqManips.Algorithm.Eval.Meta
import EqManips.Algorithm.Inject
import EqManips.Algorithm.Utils

type Var = String

-- | just an helper function
int :: Integer -> FormulaPrim
int = CInteger

-- | Public function to perform a derivation on a
-- variable.
derivateFormula :: (Formula ListForm -> EqContext (Formula ListForm))
                -> Var
                -> Formula ListForm
                -> EqContext (Formula ListForm)
derivateFormula evaluator v f =
    Formula <$> derivationRules evaluator v f

-- | real function for derivation, d was choosen
-- because I'm too lasy to type something else :]
derivationRules :: (Formula ListForm -> EqContext (Formula ListForm))
                -> String
                -> Formula ListForm
                -> EqContext FormulaPrim
derivationRules evaluator variable (Formula func) = d func variable
 where d (Meta _ m f) var = do
           Formula evaluated <- metaEval evaluator m $ Formula f
           d evaluated var

       -- Poloynome with only ^ 0, degenerated case, but
       -- must handle it
       d   (Poly _ (PolyRest _)) _ = pure $ int 0
       d f@(Poly _ (Polynome _ [])) _ = unTagFormula <$> eqFail (Formula f) Err.polynome_empty

       -- Eq:format derivate( sum( a_i * x^i ), x ) = sum( a_i * i * x ^ (i-1))
       d (Poly _ p) var = case polyDerivate p var of
            PolyRest r -> return $ coefToFormula r
            p' -> return $ poly p'


       d (Variable v) var
           | v == var = return $ int 1
           | otherwise = return $ int 0
       d (Fraction _) _ = return $ int 0
       d (CInteger _) _ = return $ int 0
       d (Indexes _ _ _) _ = return $ int 0

       d (CFloat _) _ = return $ int 0
       d (NumEntity _) _ = return $ int 0
       d (App _ f [g]) var =
           (\f' -> (app f' [g] *)) <$> d f var <*> d g var
     
       d f@(Complex _ _) _ = unTagFormula <$> eqFail (Formula f) "No complex derivation yet"
       d f@(App _ _ _) _ = unTagFormula <$> eqFail (Formula f) Err.deriv_no_multi_app
       d f@(BinOp _ _ []) _ = unTagFormula <$> eqFail (Formula f) (Err.empty_binop "derivate - ")
       d f@(BinOp _ _ [_]) _ = unTagFormula <$> eqFail (Formula f) (Err.single_binop "derivate - ")
       d f@(BinOp _ OpEq _) _ = unTagFormula <$> eqFail (Formula f) Err.deriv_no_eq_expr
       d f@(BinOp _ OpAttrib _) _ = unTagFormula <$> eqFail (Formula f) Err.deriv_no_attrib_expr
     
       -- Eq:format derivate(f + g, x) = derivate( f, x ) + 
       --                          derivate( g, x )
       d (BinOp _ OpAdd formulas) var =
           binOp OpAdd <$> mapM (flip d var) formulas
     
       -- Eq:format derivate(f - g, x) = derivate( f, x ) - 
       --                          derivate( g, x )
       d (BinOp _ OpSub formulas) var =
           binOp OpSub <$> mapM (flip d var) formulas
     
       -- Eq:format derivate( f * g, x ) =
       --      derivate( f, x ) * g + f * derivate( g, x )
       d (BinOp _ OpMul lst) var = do
          (_,_, subTrees) <- foldM mulDeriver (int 0, int 1, []) lst
          return $ binOp OpAdd subTrees
            where mulDeriver (previousDerivation, previous, rezLst) f =
                      (\derived -> ( derived
                                   , f
                                   , previousDerivation * f : previous * derived : rezLst)) <$> d f var
     
       -- Eq:format derivate( 1 / f, x ) =
       --  -derivate( f, x ) / f ^ 2
       d (BinOp _ OpDiv [(CInteger 1),f]) var =
           (\f' -> negate f' / f ** int 2) <$> d f var
     
       -- Eq:format derivate( f / g, x ) =
       --  (derivate( f, x) * g - f * derivate( g, x )) 
       --              / g ^ 2
       d (BinOp _ OpDiv (f1:lst)) var = do
          f1' <- d f1 var
          (_,_, subTrees) <- foldM divDeriver (f1', f1, []) lst
          return $ binOp OpDiv $ reverse subTrees
            where derivableDenumerator = getAny . foldf notConst (Any False)
                  notConst (Variable v) acc = Any (v == var) `mappend` acc
                  notConst _ acc = acc

                  divDeriver (previousDerivation, previous, rezLst) f
                        | derivableDenumerator f = do
                            derived <- d f var
                            let nume = (previousDerivation * f - previous * derived)
                                denom = (f ** int 2)
                            return ( nume / denom, f, denom : nume : rezLst)

                  divDeriver (previousDerivation, _, rezLst) f =
                      return ( previousDerivation / f, f
                             , f : previousDerivation : rezLst)

       -- Eq:format derivate( f ^ n, x ) = 
       --  n * derivate( f, x ) * f ^ (n - 1)
       d (BinOp _ OpPow (f1:rest)) var =
         (\f1' -> f2 * f1' * f1 ** (f2 - int 1)) <$> d f1 var
            where f2 = if length rest > 1
                          then binOp OpPow rest
                          else head rest
     
       d f@(BinOp _ _ _) _ =
           unTagFormula <$> eqFail (Formula f) "Bad binary operator biduling"
     
       -- Eq:format derivate( -f, x ) = - derivate( f, x )
       d (UnOp _ OpNegate f) var = negate <$> d f var
     
       -- Eq:format derivate(exp( f ), x) = exp(f) * derivate( f, x )
       d (UnOp _ OpExp f) var = (* exp f) <$> d f var
     
       -- Eq:format derivate( sqrt(f),x) = derivate( f, x ) / (2 * sqrt(f))
       d (UnOp _ OpSqrt f) var =
           (/ (int 2 * sqrt f)) <$> d f var
     
       -- Eq:format derivate(sin(f),x) = derivate(f,x) * cos(f)
       d (UnOp _ OpSin f) var = (* cos f) <$> d f var
     
       -- Eq:format derivate(cos(f),x) = derivate(f,x) * -sin(f)
       d (UnOp _ OpCos f) var = do
           f' <- d f var
           return $ f' * negate (sin f)
     
       -- Eq:format derivate(tan(f),x) = derivate(f,x) * 1 / cos(f) ^ 2
       d (UnOp _ OpTan f) var =
           (* (int 1 / cos f ** 2)) <$> d f var
     
       -- Eq:format derivate( asin( f ), x) = derivate(f,x) 
       --                             * 1/sqrt(1 - f^2)
       d (UnOp _ OpASin f) var =
           (* (int 1 / sqrt (int 1 - f ** int 2))) <$> d f var
     
       -- Eq:format derivate( acos( f ), x) = - derivate( f, x) *
       --          (1/sqrt( 1 - f^2))
       d (UnOp _ OpACos f) var =
           negate . (* (int 1 / sqrt (int 1 - f ** int 2))) <$> d f var
     
       -- Eq:format derivate( atan( f ),x ) = derivate( f, x) * 
       --                                  ( 1 / (1 + f^2) )
       d (UnOp _ OpATan f) var = (* (int 1 / (int 1 + f ** 2))) <$> d f var
       d (UnOp _ OpSinh f) var = (* cosh f) <$> d f var
       d (UnOp _ OpCosh f) var = (* sinh f) <$> d f var
       d (UnOp _ OpTanh f) var = (* tanh f ** 2) <$> d f var
     
       d (UnOp _ OpASinh f) var = (* (int 1 / sqrt (f ** 2 + 1))) <$> d f var
       d (UnOp _ OpACosh f) var = (* (int 1 / sqrt (f ** 2 - 1))) <$> d f var
       d (UnOp _ OpATanh f) var = (* (int 1 / (int 1 - f ** 2))) <$> d f var
       d (UnOp _ OpLn f) var = (/ f) <$> d f var
       d (UnOp _ OpLog f) var = (/ (f * log 10))<$> d f var
     
       -- | We allow deriving of lambda with only one argument...
       d (Lambda _ [([Variable v], body)]) var = do
           pushContext
           addSymbol v . Formula $ Variable var
           body' <- inject . listifyFormula $ Formula body
           popContext
           let treeIfied = unTagFormula $ treeIfyFormula body'
           body'' <- d treeIfied var
           return $ lambda [([Variable var], body'')]
     
       d f@(Lambda _ _) _ = unTagFormula <$> eqFail (Formula f) Err.deriv_lambda
     
       d f@(UnOp _ OpAbs _f) _var = unTagFormula <$>
           eqFail (Formula f) Err.deriv_no_abs
     
       d f@(UnOp _ OpFactorial _) _ = unTagFormula <$> eqFail (Formula f) Err.deriv_no_factorial
       d f@(UnOp _ OpFloor _) _ = unTagFormula <$> eqFail (Formula f) Err.deriv_floor_not_continuous 
       d f@(UnOp _ OpCeil _) _ = unTagFormula <$> eqFail (Formula f) Err.deriv_ceil_not_continuous 
       d f@(UnOp _ OpFrac _) _ = unTagFormula <$> eqFail (Formula f) Err.deriv_frac_not_continuous 
       d f@(Sum _ _i _e _w) _var = unTagFormula <$> eqFail (Formula f) Err.deriv_no_sum
       d f@(Product _ _i _e _w) _var = unTagFormula <$> eqFail (Formula f) Err.deriv_no_product
       d f@(Derivate _ _w _v) _var = unTagFormula <$> eqFail (Formula f) Err.deriv_in_deriv
       d f@(Integrate _ _i _e _w _v) _var = unTagFormula <$> eqFail (Formula f) Err.deriv_no_integration
       d f@(Matrix _ _ _ _formulas) _var = unTagFormula <$> eqFail (Formula f) Err.deriv_no_matrix
       d f@(Truth _) _ = unTagFormula <$> eqFail (Formula f) Err.deriv_no_bool
       d (Block _ _ _) _var = unTagFormula <$> eqFail (Formula $ Block 0 1 1) Err.deriv_block
       d (List _ _) _var = unTagFormula <$> eqFail (Formula $ Block 0 1 1) Err.deriv_no_list

polyDerivate :: Polynome -> String -> Polynome
polyDerivate (PolyRest _) _ = PolyRest $ CoeffInt 0
polyDerivate (Polynome _ []) _ = error Err.polynome_empty 
polyDerivate (Polynome v coefs@((c,_):xs)) var
  | v /= var =      
          let innerDerivate (coef,subPoly) = (coef, polyDerivate subPoly var)
              emptyCoeff (_, (PolyRest rest)) = isCoeffNull rest
              emptyCoeff _ = True
          in simplifyPolynome
           . Polynome v
           . filter emptyCoeff
           $ map innerDerivate coefs
    
  | otherwise = simplifyPolynome . Polynome v $ map derivator coefHead
      where coefHead = if isCoeffNull c then xs else coefs

            derivator (coef, subPoly@(Polynome _ _)) = (coef - CoeffInt 1, subPoly)
            derivator (coef, PolyRest subCoeff) =
                (coef - CoeffInt 1, PolyRest $ coef * subCoeff)
          
