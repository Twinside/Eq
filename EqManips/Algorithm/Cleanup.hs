module EqManips.Algorithm.Cleanup ( cleanup ) where

import Control.Applicative
{-import Control.Monad.State-}
import EqManips.Types
import EqManips.FormulaIterator
import EqManips.EvaluationContext

cleanup :: Formula -> EqContext Formula
cleanup = depthFirstFormula rules

int :: Int -> Formula
int = CInteger

zero :: Formula -> Bool
zero f = f == int 0 || f == CFloat 0.0

-- | List of trivial rules to simplify formula look
rules :: Formula -> EqContext Formula
-- Favor positive integer and a negate operator
-- to be able to pattern match more easily
rules cf@(CInteger i) | i < 0 = return . negate . CInteger $ negate i
                      | otherwise = return cf
-- Same as above but for floats
rules cf@(CFloat i) | i < 0 = return . negate . CFloat $ negate i
                    | otherwise = return cf
-- -(-x) = x
rules (UnOp OpNegate (UnOp OpNegate x)) = return x

-- -(0) = 0
rules (UnOp OpNegate f) | zero f = return $ int 0

-- x - (-y) <=> x + y
{-rules (BinOp OpSub x (UnOp OpNegate y)) = return $ x + y-}
-- x + (-y) <=> x - y
{-rules (BinOp OpAdd x (UnOp OpNegate y)) = return $ x - y-}

-- + cases
{-rules (BinOp OpAdd (CInteger 0) x) = return x-}
{-rules (BinOp OpAdd x (CInteger 0)) = return x-}
{-rules (BinOp OpAdd (CInteger i1) (CInteger i2)) =-}
    {-return . int $ i1 + i2-}
{-rules (BinOp OpAdd (CFloat i1) (CFloat i2)) =-}
    {-return . CFloat $ i1 + i2-}

-- - cases
{-rules (BinOp OpSub x (CInteger 0)) = return x-}
{-rules (BinOp OpSub (CInteger 0) x) = return $ negate x-}
{-rules (BinOp OpSub (CInteger i1) (CInteger i2)) =-}
    {-return . int $ i1 - i2-}
{-rules (BinOp OpSub (CFloat i1) (CFloat i2)) =-}
    {-return . CFloat $ i1 - i2-}

-- * cases
-- 0 * x = 0; x * 0 = 0
rules (BinOp OpMul fs)
    | any zero fs = return $ int 0
    | otherwise = BinOp OpMul <$> mapM rules fs

-- 1 * x = x ; x * 1 = x
{-rules (BinOp OpMul (CInteger 1) x) = return x-}
{-rules (BinOp OpMul x (CInteger 1)) = return x-}
-- evaluation of constants
{-rules (BinOp OpMul (CInteger i1) (CInteger i2)) =-}
    {-return . int $ i1 * i2-}
{-rules (BinOp OpMul (CFloat i1) (CFloat i2)) =-}
    {-return . CFloat $ i1 * i2-}

-- / cases
{-rules (BinOp OpDiv (CInteger 0) _) = return $ int 0-}
{-rules (BinOp OpDiv x (CInteger 1)) = return x-}
{-rules f@(BinOp OpDiv _ (CInteger 0)) = eqFail f "Division by 0, WTF (BBQ)!"-}

-- POWER function
{-rules (BinOp OpPow _ (CInteger 0)) = return $ int 1-}
{-rules (BinOp OpPow x (CInteger 1)) = return x-}

-- Some Trigonometric transformations...
rules (UnOp OpSin (CInteger 0)) = return $ int 0
rules (UnOp OpSin (NumEntity Pi)) = return $ int 0
-- Here we handle the more general case, for 2k * pi + 1
-- and 2k * pi
{-rules (UnOp OpSin (BinOp OpMul (NumEntity Pi) (CInteger i))) -}
    {-| i `mod` 2 == 0 = return $ int 0-}
    {-| otherwise = return $ int 1-}
{-rules (UnOp OpSin (BinOp OpMul (CInteger i) (NumEntity Pi))) -}
    {-| i `mod` 2 == 0 = return $ int 0-}
    {-| otherwise = return $ int 1-}

rules (UnOp OpCos (CInteger 0)) = return $ int 1
rules (UnOp OpCos (NumEntity Pi)) = return $ int (-1)

rules f = return f

