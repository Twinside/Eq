module EqManips.Algorithm.Derivative( derivate ) where

import EqManips.Types
import EqManips.EvaluationContext

type Variable = String

int :: Int -> Formula
int = CInteger

derivate :: Formula -> Variable -> EqContext Formula
derivate f v = d f v

d :: Formula -> String -> EqContext Formula
d (Variable v) var
    | v == var = return $ int 1
    | otherwise = return $ int 0
d (CInteger _) _ = return $ int 0
d (CFloat _) _ = return $ int 0
d (App f [g]) var = do
    f' <- d f var
    g' <- d g var
    return $ (App f' [g]) * g'

d (App _ _) _ =
    fail "Ok, now solution for app with multi argument"

-- EQ: derivate(f + g, x) = derivate( f, x ) + 
--                          derivate( g, x )
d (BinOp OpAdd  f1 f2) var = do
    f1' <- d f1 var
    f2' <- d f2 var
    return $ f1' + f2'

-- EQ: derivate(f - g, x) = derivate( f, x ) - 
--                          derivate( g, x )
d (BinOp OpSub f1 f2) var = do
    f1' <- d f1 var
    f2' <- d f2 var
    return $ f1' + f2'

-- EQ: derivate( f * g, x ) =
--      derivate( f, x ) * g + f + derivate( g, x )
d (BinOp OpMul f1 f2) var = do
    f1' <- d f1 var
    f2' <- d f2 var
    return $ f1' * f2 + f1 * f2'

-- EQ: derivate( 1 / f ) =
--  -derivate( f, x ) / f ^ 2
d (BinOp OpDiv (CInteger 1) f) var = do
    f' <- d f var
    return $ (negate f') / f ** (int 2)

-- EQ: derivate( f / g, x ) =
--  (derivate( f, x) * g - f * derivate( g, x )) 
--              / g ^ 2
d (BinOp OpDiv f1 f2) var = do
   f1' <- d f1 var
   f2' <- d f2 var
   return $ (f1' * f2 - f1 * f2') / 
               (f2 ** int 2)

-- EQ: derivate( f ^ n, x ) = 
--  n * derivate( f, x ) * f ^ (n - 1)
d (BinOp OpPow f1 f2) var = do
  f1' <- d f1 var
  return $ f2 * f1' * f1 ** (f2 - (int 1))

-- EQ: derivate( -f, x ) = - derivate( f, x )
d (UnOp OpNegate f) var = do
    f' <- d f var
    return $ negate f'

-- EQ: derivate(exp( f ), x) = exp(f) * derivate( f, x )
d (UnOp OpExp f) var = do
    f' <- d f var
    return $ f' * exp f

-- EQ: derivate( sqrt(f),x) = derivate( f, x ) / (2 * sqrt(f))
d (UnOp OpSqrt f) var = do
    f' <- d f var
    return $ f' / (int 2 * sqrt f)

-- EQ: derivate(sin(f),x) = derivate(f,x) * cos(f)
d (UnOp OpSin f) var = do
    f' <- d f var
    return $ f' * cos f

-- EQ: derivate(cos(f),x) = derivate(f,x) * -sin(f)
d (UnOp OpCos f) var = do
    f' <- d f var
    return $ f' * negate (sin f)

-- EQ: derivate(tan(f),x) = derivate(f,x) * 1 / cos(f) ^ 2
d (UnOp OpTan f) var = do
    f' <- d f var
    return $ f' * (int 1 / cos f ** 2)

-- EQ: derivate( asin( f ), x) = derivate(f,x) 
--                             * 1/sqrt(1 - f^2)
d (UnOp OpASin f) var = do
    f' <- d f var
    return $ f' * (int 1 / sqrt (int 1 - f ** int 2))
-- EQ: derivate( acos( f ), x) = - derivate( f, x) *
--          (1/sqrt( 1 - f^2))
d (UnOp OpACos f) var = do
    f' <- d f var
    return . negate $ f' * (int 1 / sqrt (int 1 - f ** int 2))

-- EQ: derivate( atan( f ),x ) = derivate( f, x) * 
--                                  ( 1 / (1 + f^2) )
d (UnOp OpATan f) var = do
    f' <- d f var
    return $ f' * (int 1 / (int 1 + f ** 2))

d (UnOp OpSinh f) var = do
    f' <- d f var
    return $ f' * (UnOp OpCosh f)

d (UnOp OpCosh f) var = do
    f' <- d f var
    return $ f' * (UnOp OpSinh f)

d (UnOp OpTanh f) var = do
    f' <- d f var
    return $ f' * (int 1)

d (UnOp OpASinh f) var = do
    f' <- d f var
    return $ f' * (int 1 / sqrt (f ** 2 + 1))

d (UnOp OpACosh f) var = do
    f' <- d f var
    return $ f' * (int 1 / sqrt (f ** 2 - 1))

d (UnOp OpATanh f) var = do
    f' <- d f var
    return $ f' * (int 1 / (int 1 - f ** 2))

d (BinOp OpEq _f1 _f2) _var =
    fail " '=' For the moment we don't know what to do with it"
d (UnOp OpLn _f) _var =
    fail "No position for Ln for now"
d (UnOp OpLog _f) _var =
    fail "No position for Log for now"
d (UnOp OpAbs _f) _var =
    fail "abs is derivable? I don't think so"

d (Sum _i _e _w) _var =
    fail "Oki, deriving sums is not defined..."

d (Product _i _e _w) _var =
    fail "Deriving product is undefined. Sorry. Really."
d (Derivate _w _v) _var =
    fail ""
d (Integrate _i _e _w _v) _var =
    fail  ""
d (Matrix _ _ _formulas) _var =
    fail "Can't derive a matrix."
d (Block _ _ _) _var =
    fail $ "hmm, you are trying to derivate a function used\n"
        ++ "to debug the equation renderer. Here is the result :\n"
        ++ "@\n"
        ++ "\n"
        ++ "Yes, it doesn't mean anything"

