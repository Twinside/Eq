module EqManips.Algorithm.Cleanup ( cleanup ) where

import Control.Monad.State
import EqManips.Types
import EqManips.FormulaIterator

recurse :: (Monad m) => Formula -> m Formula
recurse = formulaIterate cleanup

int :: Int -> Formula
int = CInteger

cleanup :: (Monad m) => Formula -> m Formula
-- -(-x) <=> x
cleanup (UnOp OpNegate (UnOp OpNegate x)) = recurse x
-- x - (-y) <=> x + y
cleanup (BinOp OpSub x (UnOp OpNegate y)) = recurse $ x + y
-- x + (-y) <=> x - y
cleanup (BinOp OpAdd x (UnOp OpNegate y)) = recurse $ x - y

-- + cases
cleanup (BinOp OpAdd (CInteger 0) x) = recurse x
cleanup (BinOp OpAdd x (CInteger 0)) = recurse x
cleanup (BinOp OpAdd (CInteger i1) (CInteger i2)) =
    recurse . int $ i1 + i2
cleanup (BinOp OpAdd (CFloat i1) (CFloat i2)) =
    recurse . CFloat $ i1 + i2

-- - cases
cleanup (BinOp OpSub x (CInteger 0)) = do recurse x
cleanup (BinOp OpSub (CInteger i1) (CInteger i2)) =
    recurse . int $ i1 - i2
cleanup (BinOp OpSub (CFloat i1) (CFloat i2)) =
    recurse . CFloat $ i1 - i2

-- * cases
-- 0 * x = 0; x * 0 = 0
cleanup (BinOp OpMul (CInteger 0) _) = recurse $ int 0
cleanup (BinOp OpMul _ (CInteger 0)) = recurse $ int 0
-- 1 * x = x ; x * 1 = x
cleanup (BinOp OpMul (CInteger 1) x) = recurse x
cleanup (BinOp OpMul x (CInteger 1)) = recurse x
-- evaluation of constants
cleanup (BinOp OpMul (CInteger i1) (CInteger i2)) =
    recurse . int $ i1 * i2
cleanup (BinOp OpMul (CFloat i1) (CFloat i2)) =
    recurse . CFloat $ i1 * i2

-- / cases
cleanup (BinOp OpDiv (CInteger 0) _) = recurse $ int 0
cleanup (BinOp OpDiv x (CInteger 1)) = recurse x
cleanup (BinOp OpDiv _ (CInteger 0)) = fail "Division by 0, WTF (BBQ)!"

-- POWER function
cleanup (BinOp OpPow _ (CInteger 0)) = return $ int 1
cleanup (BinOp OpPow x (CInteger 1)) = recurse x

-- Some Trigonometric transformations...
cleanup (UnOp OpSin (CInteger 0)) = return $ int 0
cleanup (UnOp OpSin (NumEntity Pi)) = return $ int 0
-- Here we handle the more general case, for 2k * pi + 1
-- and 2k * pi
cleanup (UnOp OpSin (BinOp OpMul (NumEntity Pi) (CInteger i))) 
    | i `mod` 2 == 0 = return $ int 0
    | otherwise = return $ int 1
cleanup (UnOp OpSin (BinOp OpMul (CInteger i) (NumEntity Pi))) 
    | i `mod` 2 == 0 = return $ int 0
    | otherwise = return $ int 1

cleanup (UnOp OpCos (CInteger 0)) = return $ int 1
cleanup (UnOp OpCos (NumEntity Pi)) = return $ int (-1)

cleanup f | isFormulaLeaf f = return f
          | otherwise = recurse f

