module EqManips.Algorithm.Derivative( derivate
                                    , Var ) where

import Control.Applicative
import EqManips.Types
import EqManips.EvaluationContext
import EqManips.FormulaIterator
import EqManips.Algorithm.MetaEval

type Var = String

-- | just an helper function
int :: Int -> Formula
int = CInteger

-- | Public function to perform a derivation on a
-- variable.
derivate :: Formula -> Var -> EqContext Formula
derivate f v = d f v

-- | real function for derivation, d was choosen
-- because I'm too lasy to type something else :]
d :: Formula -> String -> EqContext Formula
d (Meta m f) var = metaEval m f >>= flip d var
d (Variable v) var
    | v == var = return $ int 1
    | otherwise = return $ int 0
d (CInteger _) _ = return $ int 0
d (CFloat _) _ = return $ int 0
d (NumEntity _) _ = return $ int 0
d (App f [g]) var =
    (\f' g' -> (App f' [g]) * g') <$> d f var <*> d g var


d f@(App _ _) _ =
    eqFail f "Ok, now solution for app with multi argument"

d f@(BinOp _ []) _ = eqFail f "Binary op with no param"
d f@(BinOp _ [_]) _ = eqFail f "Binary op with only one param"
d f@(BinOp OpEq _) _ = eqFail f "Can't derivate expression with a '='"

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
    (\f' -> (negate f') / f ** (int 2)) <$> d f var

-- Eq:format derivate( f / g, x ) =
--  (derivate( f, x) * g - f * derivate( g, x )) 
--              / g ^ 2
d (BinOp OpDiv [f1,f2]) var =
    if derivableDenumerator
       then (\f1' f2' -> (f1' * f2 - f1 * f2') / (f2 ** int 2))
               <$> d f1 var <*> d f2 var
        else (\f1' -> f1' / f2) <$> d f1 var
 where derivableDenumerator = foldf notConst False f2
       notConst (Variable v) acc = v == var || acc
       notConst _ acc = acc


-- Eq:format derivate( f ^ n, x ) = 
--  n * derivate( f, x ) * f ^ (n - 1)
d (BinOp OpPow [f1,f2]) var =
  (\f1' -> f2 * f1' * f1 ** (f2 - (int 1))) <$> d f1 var

d (BinOp op (x:x2:xs)) var =
    d (BinOp op [x, BinOp op $ x2:xs]) var

-- Eq:format derivate( -f, x ) = - derivate( f, x )
d (UnOp OpNegate f) var = negate <$> d f var

-- Eq:format derivate(exp( f ), x) = exp(f) * derivate( f, x )
d (UnOp OpExp f) var = (\f' -> f' * exp f) <$> d f var

-- Eq:format derivate( sqrt(f),x) = derivate( f, x ) / (2 * sqrt(f))
d (UnOp OpSqrt f) var =
    (\f' -> f' / (int 2 * sqrt f)) <$> d f var

-- Eq:format derivate(sin(f),x) = derivate(f,x) * cos(f)
d (UnOp OpSin f) var =
    (\f' -> f' * cos f) <$> d f var

-- Eq:format derivate(cos(f),x) = derivate(f,x) * -sin(f)
d (UnOp OpCos f) var = do
    f' <- d f var
    return $ f' * negate (sin f)

-- Eq:format derivate(tan(f),x) = derivate(f,x) * 1 / cos(f) ^ 2
d (UnOp OpTan f) var =
    (\f' -> f' * (int 1 / cos f ** 2)) <$> d f var

-- Eq:format derivate( asin( f ), x) = derivate(f,x) 
--                             * 1/sqrt(1 - f^2)
d (UnOp OpASin f) var =
    (\f' -> f' * (int 1 / sqrt (int 1 - f ** int 2))) <$> d f var

-- Eq:format derivate( acos( f ), x) = - derivate( f, x) *
--          (1/sqrt( 1 - f^2))
d (UnOp OpACos f) var =
    (\f' -> negate $ f' * (int 1 / sqrt (int 1 - f ** int 2))) <$> d f var

-- Eq:format derivate( atan( f ),x ) = derivate( f, x) * 
--                                  ( 1 / (1 + f^2) )
d (UnOp OpATan f) var = (\f' -> f' * (int 1 / (int 1 + f ** 2))) <$> d f var
d (UnOp OpSinh f) var = (\f' -> f' * cosh f) <$> d f var
d (UnOp OpCosh f) var = (\f' -> f' * sinh f) <$> d f var
d (UnOp OpTanh f) var = (\f' -> f' * tanh f ** 2) <$> d f var

d (UnOp OpASinh f) var = (\f' -> f' * (int 1 / sqrt (f ** 2 + 1))) <$> d f var
d (UnOp OpACosh f) var = (\f' -> f' * (int 1 / sqrt (f ** 2 - 1))) <$> d f var
d (UnOp OpATanh f) var = (\f' -> f' * (int 1 / (int 1 - f ** 2))) <$> d f var
d fo@(UnOp OpLn f) var = (\f' -> f' / fo) <$> d f var

d f@(UnOp OpLog _f) _var =
    eqFail f "No position for Log for now"
d f@(UnOp OpAbs _f) _var =
    eqFail f "abs is derivable? I don't think so"

d f@(Sum _i _e _w) _var =
    eqFail f "Oki, deriving sums is not defined..."

d f@(Product _i _e _w) _var =
    eqFail f "Deriving product is undefined. Sorry. Really."

d f@(Derivate _w _v) _var =
    eqFail f "Derivate a derivative, what to do?"

d f@(Integrate _i _e _w _v) _var =
    eqFail f "Derivate an integration, what to do?"

d f@(Matrix _ _ _formulas) _var =
    eqFail f "Deriving a Matrix, what to do?"

d f@(Lambda _) _ =
    eqFail f "Deriving lambdas"

d (Block _ _ _) _var =
    eqFail (Block 0 1 1)
         $ "Deriving a debug block"
