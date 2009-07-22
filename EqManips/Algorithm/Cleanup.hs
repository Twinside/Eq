module EqManips.Algorithm.Cleanup ( simplify
                                  ) where

import Control.Monad.State
import EqManips.Types
import EqManips.FormulaIterator

simplify :: Formula -> Formula
-- + cases
simplify (BinOp OpAdd (CInteger 0) x) = do return x
simplify (BinOp OpAdd x (CInteger 0)) = do return x
simplify (BinOp OpAdd (CInteger i1) (CInteger i2)) = do
    return . CInteger $ i1 + i2
simplify (BinOp OpAdd (CFloat i1) (CFloat i2)) = do
    return . CFloat $ i1 + i2

-- - cases
simplify (BinOp OpSub x (CInteger 0)) = do return x
simplify (BinOp OpSub (CInteger i1) (CInteger i2)) = do
    return . CInteger $ i1 - i2
simplify (BinOp OpSub (CFloat i1) (CFloat i2)) = do
    return . CFloat $ i1 - i2

-- * cases
simplify (BinOp OpMul (CInteger 0) _) = return (CInteger 0)
simplify (BinOp OpMul (CInteger 1) x) = return x
simplify (BinOp OpMul x (CInteger 1)) = return x
simplify (BinOp OpMul _ (CInteger 0)) = return (CInteger 0)
simplify (BinOp OpMul (CInteger i1) (CInteger i2)) = do
    return . CInteger $ i1 * i2
simplify (BinOp OpMul (CFloat i1) (CFloat i2)) = do
    return . CFloat $ i1 * i2

-- / cases
simplify (BinOp OpDiv (CInteger 0) _) = return (CInteger 0)
simplify (BinOp OpDiv _ (CInteger 0)) = fail ""
simplify (BinOp OpDiv x (CInteger 1)) = return x

-- POWER function
simplify (BinOp OpPow _ (CInteger 0)) = return $ CInteger 1
simplify (BinOp OpPow x (CInteger 1)) = return x

simplify f | isFormulaLeaf f = f
           | otherwise = formulaIterate simplify f


