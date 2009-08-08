module EqManips.Algorithm.Cleanup ( cleanup ) where

import EqManips.Types
import EqManips.FormulaIterator
import EqManips.Algorithm.Utils

type BiRuler = Formula -> Formula -> Either Formula (Formula, Formula)

cleanup :: Formula -> Formula
cleanup = depthFirstFormula `asAMonad` rules

int :: Int -> Formula
int = CInteger

zero :: Formula -> Bool
zero f = f == int 0 || f == CFloat 0.0

----------------------------------------------
----                '+'
----------------------------------------------
-- | Addition rules, everything
-- concerning the '+' operator
add :: BiRuler 
-- What's the point?
add (CInteger 0) x = Left x
add x (CInteger 0) = Left x
add (CFloat 0) x = Left x
add x (CFloat 0) = Left x

add (CInteger a) (CInteger b) = Left . int $ a + b
-- x + (-y) <=> x - y
{-rules (BinOp OpAdd x (UnOp OpNegate y)) = return $ x - y-}
add x y = Right (x,y)

----------------------------------------------
----                '-'
----------------------------------------------
-- | Substraction rules
sub :: BiRuler
sub x (CInteger 0) = Left x
sub (CInteger 0) x = Left $ negate x
sub (CInteger i1) (CInteger i2) = Left . int $ i1 - i2
-- x - (-y) <=> x + y
{-rules (BinOp OpSub x (UnOp OpNegate y)) = return $ x + y-}
sub x y = Right (x,y)

----------------------------------------------
----                '*'
----------------------------------------------
mul :: BiRuler
mul (CInteger 1) x = Left x
mul x (CInteger 1) = Left x
mul (CInteger i1) (CInteger i2) = Left . int $ i1 + i2
mul x y = Right (x,y)

----------------------------------------------
----                '**'
----------------------------------------------
power :: BiRuler
power _ (CInteger 0) = Left $ int 1
power x (CInteger 1) = Left x
power x y = Right (x,y)

----------------------------------------------
----                '/'
----------------------------------------------
divide :: Formula -> Formula -> Either Formula (Formula,Formula)
divide (CInteger 0) _ = Left $ int 0
divide x (CInteger 1) = Left x
divide x y = Right (x,y)

----------------------------------------------
----                'sinus'
----------------------------------------------
sinus :: Formula -> Formula
sinus (CInteger 0) = int 0
sinus (NumEntity Pi) = int 0
-- Here we handle the more general case, for 2k * pi + 1
-- and 2k * pi
sinus (BinOp OpMul [NumEntity Pi, CInteger i])
    | i `mod` 2 == 0 = int 0
    | otherwise = int 1
sinus (BinOp OpMul [CInteger i, NumEntity Pi])
    | i `mod` 2 == 0 = int 0
    | otherwise = int 1
sinus i = sin i

----------------------------------------------
----                'cosinus'
----------------------------------------------
cosinus :: Formula -> Formula
cosinus (CInteger 0) = int 1
cosinus (NumEntity Pi) = int (-1)
cosinus i = cos i


reOp :: BinOperator -> [Formula] -> Formula
reOp _ [] = error "reOp Empty formula? WTF"
reOp _ [x] = x
reOp op lst = BinOp op lst

---------------------------------------------
---- Linking all the rules together
---------------------------------------------
rules :: Formula -> Formula
rules (UnOp OpSin f) = sinus f
rules (UnOp OpCos f) = cosinus f
rules (BinOp OpAdd fs) = reOp OpAdd $ biAssoc add fs
rules (BinOp OpSub fs) = reOp OpSub $ biAssoc sub fs
rules (BinOp OpDiv fs) = reOp OpDiv $ biAssoc divide fs
rules (BinOp OpPow fs) = reOp OpPow $ biAssoc power fs
rules (BinOp OpMul fs)
    -- 0 * x or x * 0 in a multiplication result in 0
    | any zero fs = int 0
    | otherwise = reOp OpMul $ biAssoc mul fs

-- Favor positive integer and a negate operator
-- to be able to pattern match more easily
rules cf@(CInteger i) | i < 0 = negate . CInteger $ negate i
                      | otherwise = cf
-- Same as above but for floats
rules cf@(CFloat i) | i < 0 = negate . CFloat $ negate i
                    | otherwise = cf
-- -(-x) = x
rules (UnOp OpNegate (UnOp OpNegate x)) = x

-- -(0) = 0
rules (UnOp OpNegate f) | zero f = int 0


rules f = f

