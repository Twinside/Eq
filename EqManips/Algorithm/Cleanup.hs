module EqManips.Algorithm.Cleanup ( cleanup ) where

import Control.Applicative
{-import Control.Monad.State-}
import EqManips.Types
import EqManips.FormulaIterator
import EqManips.EvaluationContext

type BiRuler = Formula -> Formula -> Either Formula (Formula, Formula)

cleanup :: Formula -> EqContext Formula
cleanup = depthFirstFormula rules

int :: Int -> Formula
int = CInteger

zero :: Formula -> Bool
zero f = f == int 0 || f == CFloat 0.0

biAssoc :: (a -> a -> Either a (a,a)) -> [a] -> [a]
biAssoc _ [] = []
biAssoc _ [x] = [x]
biAssoc f [x,y] = case f x y of
    Left v -> [v]
    Right (v1, v2) -> [v1, v2]
biAssoc f (x:y:xs) = case f x y of
    Left v -> biAssoc f (v:xs)
    Right (v1, v2) -> v1 : biAssoc f (v2:xs)

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
divide :: Formula -> Formula -> EqContext (Either Formula (Formula,Formula))
-- | List of trivial rules to simplify formula look
divide (CInteger 0) _ = return . Left $ int 0
divide x (CInteger 1) = return $ Left x
divide f (CInteger 0) = do
    eqFail (BinOp OpDiv [f, int 0]) "Division by 0, WTF (BBQ)!"
    return . Left $ Block 1 1 1
divide x y = return $ Right (x,y)

----------------------------------------------
----                'sinus'
----------------------------------------------
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

----------------------------------------------
----                'cosinus'
----------------------------------------------
cosinus (CInteger 0) = int 1
cosinus (NumEntity Pi) = int (-1)

---------------------------------------------
---- Linking all the rules together
---------------------------------------------
rules :: Formula -> EqContext Formula
rules (UnOp OpSin f) = return $ sinus f
rules (UnOp OpCos f) = return $ cosinus f
rules (BinOp OpAdd fs) = return . BinOp OpAdd $ biAssoc add fs
rules (BinOp OpSub fs) = return . BinOp OpSub $ biAssoc sub fs
rules (BinOp OpDiv fs) = return . BinOp OpDiv $ biAssoc divide fs
rules (BinOp OpPow fs) = return . BinOp OpPow $ biAssoc power fs
rules (BinOp OpMul fs)
    -- 0 * x or x * 0 in a multiplication result in 0
    | any zero fs = return $ int 0
    | otherwise = return . BinOp OpMul $ biAssoc mul fs

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


rules f = return f

