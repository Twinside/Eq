module EqManips.Algorithm.Derivative( derivate
                                    , Var ) where

import Control.Applicative
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
derivate :: (Formula ListForm -> EqContext (Formula ListForm))
         -> Var
         -> Formula TreeForm
         -> EqContext (Formula ListForm)
derivate evaluator v f = do
    derived <- derivationRules evaluator v f
    let finalForm = listifyFormula $ Formula derived
    return $ sortFormula finalForm

-- | real function for derivation, d was choosen
-- because I'm too lasy to type something else :]
derivationRules :: (Formula ListForm -> EqContext (Formula ListForm))
                -> String
                -> Formula TreeForm
                -> EqContext FormulaPrim
derivationRules evaluator variable (Formula func) = d func variable
 where d (Meta m f) var = do
           let listified = listifyFormula $ Formula f
           formula <- metaEval evaluator m listified
           let (Formula evaluated) = treeIfyFormula formula
           d evaluated var

       -- Poloynome with only ^ 0, degenerated case, but
       -- must handle it
       d   (Poly (PolyRest _)) _ = pure $ int 0
       d f@(Poly (Polynome _ [])) _ = unTagFormula <$> eqFail (Formula f) Err.polynome_empty

       -- Eq:format derivate( sum( a_i * x^i ), x ) = sum( a_i * i * x ^ (i-1))
       d (Poly p) var = case polyDerivate p var of
            PolyRest r -> return $ coefToFormula r
            p' -> return $ Poly p'


       d (Variable v) var
           | v == var = return $ int 1
           | otherwise = return $ int 0
       d (Fraction _) _ = return $ int 0
       d (CInteger _) _ = return $ int 0
       d (CFloat _) _ = return $ int 0
       d (NumEntity _) _ = return $ int 0
       d (App f [g]) var =
           (\f' -> (App f' [g] *)) <$> d f var <*> d g var
     
       d f@(Complex _) _ = unTagFormula <$> eqFail (Formula f) "No complex derivation yet"
       d f@(App _ _) _ = unTagFormula <$> eqFail (Formula f) Err.deriv_no_multi_app
       d f@(BinOp _ []) _ = unTagFormula <$> eqFail (Formula f) (Err.empty_binop "derivate - ")
       d f@(BinOp _ [_]) _ = unTagFormula <$> eqFail (Formula f) (Err.single_binop "derivate - ")
       d f@(BinOp OpEq _) _ = unTagFormula <$> eqFail (Formula f) Err.deriv_no_eq_expr
       d f@(BinOp OpAttrib _) _ = unTagFormula <$> eqFail (Formula f) Err.deriv_no_attrib_expr
     
       -- Eq:format derivate(f + g, x) = derivate( f, x ) + 
       --                          derivate( g, x )
       d (BinOp OpAdd formulas) var =
           BinOp OpAdd <$> mapM (flip d var) formulas
     
       -- Eq:format derivate(f - g, x) = derivate( f, x ) - 
       --                          derivate( g, x )
       d (BinOp OpSub formulas) var =
           BinOp OpSub <$> mapM (flip d var) formulas
     
       -- Eq:format derivate( f * g, x ) =
       --      derivate( f, x ) * g + f + derivate( g, x )
       d (BinOp OpMul [f1,f2]) var =
           (\f1' f2' -> f1' * f2 + f1 * f2') <$> d f1 var <*> d f2 var
     
       -- Eq:format derivate( 1 / f, x ) =
       --  -derivate( f, x ) / f ^ 2
       d (BinOp OpDiv [(CInteger 1),f]) var =
           (\f' -> negate f' / f ** int 2) <$> d f var
     
       -- Eq:format derivate( f / g, x ) =
       --  (derivate( f, x) * g - f * derivate( g, x )) 
       --              / g ^ 2
       d (BinOp OpDiv [f1,f2]) var =
           if derivableDenumerator
              then (\f1' f2' -> (f1' * f2 - f1 * f2') / (f2 ** int 2))
                      <$> d f1 var <*> d f2 var
               else (/ f2) <$> d f1 var
        where derivableDenumerator = getAny $ foldf notConst (Any False) f2
              notConst (Variable v) acc = Any (v == var) `mappend` acc
              notConst _ acc = acc
     
     
       -- Eq:format derivate( f ^ n, x ) = 
       --  n * derivate( f, x ) * f ^ (n - 1)
       d (BinOp OpPow [f1,f2]) var =
         (\f1' -> f2 * f1' * f1 ** (f2 - int 1)) <$> d f1 var
     
       d (BinOp op (x:x2:xs)) var =
           d (BinOp op [x, BinOp op $ x2:xs]) var
     
       -- Eq:format derivate( -f, x ) = - derivate( f, x )
       d (UnOp OpNegate f) var = negate <$> d f var
     
       -- Eq:format derivate(exp( f ), x) = exp(f) * derivate( f, x )
       d (UnOp OpExp f) var = (* exp f) <$> d f var
     
       -- Eq:format derivate( sqrt(f),x) = derivate( f, x ) / (2 * sqrt(f))
       d (UnOp OpSqrt f) var =
           (/ (int 2 * sqrt f)) <$> d f var
     
       -- Eq:format derivate(sin(f),x) = derivate(f,x) * cos(f)
       d (UnOp OpSin f) var = (* cos f) <$> d f var
     
       -- Eq:format derivate(cos(f),x) = derivate(f,x) * -sin(f)
       d (UnOp OpCos f) var = do
           f' <- d f var
           return $ f' * negate (sin f)
     
       -- Eq:format derivate(tan(f),x) = derivate(f,x) * 1 / cos(f) ^ 2
       d (UnOp OpTan f) var =
           (* (int 1 / cos f ** 2)) <$> d f var
     
       -- Eq:format derivate( asin( f ), x) = derivate(f,x) 
       --                             * 1/sqrt(1 - f^2)
       d (UnOp OpASin f) var =
           (* (int 1 / sqrt (int 1 - f ** int 2))) <$> d f var
     
       -- Eq:format derivate( acos( f ), x) = - derivate( f, x) *
       --          (1/sqrt( 1 - f^2))
       d (UnOp OpACos f) var =
           negate . (* (int 1 / sqrt (int 1 - f ** int 2))) <$> d f var
     
       -- Eq:format derivate( atan( f ),x ) = derivate( f, x) * 
       --                                  ( 1 / (1 + f^2) )
       d (UnOp OpATan f) var = (* (int 1 / (int 1 + f ** 2))) <$> d f var
       d (UnOp OpSinh f) var = (* cosh f) <$> d f var
       d (UnOp OpCosh f) var = (* sinh f) <$> d f var
       d (UnOp OpTanh f) var = (* tanh f ** 2) <$> d f var
     
       d (UnOp OpASinh f) var = (* (int 1 / sqrt (f ** 2 + 1))) <$> d f var
       d (UnOp OpACosh f) var = (* (int 1 / sqrt (f ** 2 - 1))) <$> d f var
       d (UnOp OpATanh f) var = (* (int 1 / (int 1 - f ** 2))) <$> d f var
       d fo@(UnOp OpLn f) var = (/ fo) <$> d f var
     
       -- | We allow deriving of lambda with only one argument...
       d (Lambda [([Variable v], body)]) var = do
           pushContext
           addSymbol v . Formula $ Variable var
           body' <- inject . listifyFormula $ Formula body
           popContext
           let treeIfied = unTagFormula $ treeIfyFormula body'
           body'' <- d treeIfied var
           return $ Lambda [([Variable var], body'')]
     
       d f@(Lambda _) _ = unTagFormula <$> eqFail (Formula f) Err.deriv_lambda
     
       d f@(UnOp OpLog _f) _var = unTagFormula <$>
           eqFail (Formula f) Err.deriv_no_log
       d f@(UnOp OpAbs _f) _var = unTagFormula <$>
           eqFail (Formula f) Err.deriv_no_abs
     
       d f@(UnOp OpFactorial _) _ = unTagFormula <$> eqFail (Formula f) Err.deriv_no_factorial
       d f@(UnOp OpFloor _) _ = unTagFormula <$> eqFail (Formula f) Err.deriv_floor_not_continuous 
       d f@(UnOp OpCeil _) _ = unTagFormula <$> eqFail (Formula f) Err.deriv_ceil_not_continuous 
       d f@(UnOp OpFrac _) _ = unTagFormula <$> eqFail (Formula f) Err.deriv_frac_not_continuous 
       d f@(Sum _i _e _w) _var = unTagFormula <$> eqFail (Formula f) Err.deriv_no_sum
       d f@(Product _i _e _w) _var = unTagFormula <$> eqFail (Formula f) Err.deriv_no_product
       d f@(Derivate _w _v) _var = unTagFormula <$> eqFail (Formula f) Err.deriv_in_deriv
       d f@(Integrate _i _e _w _v) _var = unTagFormula <$> eqFail (Formula f) Err.deriv_no_integration
       d f@(Matrix _ _ _formulas) _var = unTagFormula <$> eqFail (Formula f) Err.deriv_no_matrix
       d f@(Truth _) _ = unTagFormula <$> eqFail (Formula f) Err.deriv_no_bool
       d (Block _ _ _) _var = unTagFormula <$> eqFail (Formula $ Block 0 1 1) Err.deriv_block

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
          
