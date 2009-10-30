module EqManips.Algorithm.Cleanup ( cleanup, cleanupRules ) where

import EqManips.Types
import EqManips.FormulaIterator
import EqManips.Algorithm.Utils

type BiRuler = FormulaPrim -> FormulaPrim -> Either FormulaPrim (FormulaPrim, FormulaPrim)

cleanup :: Formula anyForm -> Formula anyForm
cleanup = depthFirstFormula `asAMonad` (Formula . rules . unTagFormula)

cleanupRules :: Formula anyForm -> Formula anyForm
cleanupRules (Formula a) = Formula $ rules a

int :: Integer -> FormulaPrim
int = CInteger

zero :: FormulaPrim -> Bool
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
-- Eq:format (1/denom) * x = x / denom
mul (BinOp OpDiv [CInteger 1, denom]) x = Left $ x / denom
-- Eq:format x * (1/denom) = x / denom
mul x (BinOp OpDiv [CInteger 1, denom]) = Left $ x / denom

-- Eq:format (-1/denom) * x = -x / denom
mul (BinOp OpDiv [UnOp OpNegate (CInteger 1), denom]) x = Left $ (negate x) / denom
-- Eq:format x * (-1/denom) = -x / denom
mul x (BinOp OpDiv [UnOp OpNegate (CInteger 1), denom]) = Left $ (negate x) / denom

-- Eq:format a ^ n * a ^ m = a ^ (n + m)
mul (BinOp OpPow [a, n]) (BinOp OpPow [b, m]) | a == b = Left $ a ** (n + m)
mul (CInteger 1) x = Left x
mul x (CInteger 1) = Left x
mul (CFloat 1.0) x = Left x
mul x (CFloat 1.0) = Left x
mul (CInteger i1) (CInteger i2) = Left . int $ i1 * i2
mul (BinOp OpDiv [a,b]) (BinOp OpDiv [c,d])
    | b == d = Left $ (a * c) / d
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
divide :: BiRuler
divide (CInteger 0) _ = Left $ int 0
divide x (CInteger 1) = Left x
divide x y = Right (x,y)

----------------------------------------------
----                'sinus'
----------------------------------------------
sinus :: FormulaPrim -> FormulaPrim
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
cosinus :: FormulaPrim -> FormulaPrim
cosinus (CInteger 0) = int 1
cosinus (NumEntity Pi) = int (-1)
cosinus i = cos i

--------------------------------------------------
----            'exp'
--------------------------------------------------
exponential :: FormulaPrim -> FormulaPrim
exponential (CInteger 0) = int 1
exponential (CFloat 0.0) = int 1
exponential f = exp f

reOp :: BinOperator -> [FormulaPrim] -> FormulaPrim
reOp _ [] = error "reOp Empty formula? WTF"
reOp _ [x] = x
reOp op lst = BinOp op lst

---------------------------------------------
---- Linking all the rules together
---------------------------------------------
rules :: FormulaPrim -> FormulaPrim
rules (UnOp OpSin f) = sinus f
rules (UnOp OpCos f) = cosinus f
rules (UnOp OpExp f) = exponential f
rules (BinOp OpAdd fs) = reOp OpAdd $ biAssoc add add fs
rules (BinOp OpSub fs) = reOp OpSub $ biAssoc sub add fs
rules (BinOp OpDiv fs) = reOp OpDiv $ biAssoc divide mul fs
rules (BinOp OpPow fs) = reOp OpPow $ biAssoc power mul fs
rules (BinOp OpMul fs)
    -- 0 * x or x * 0 in a multiplication result in 0
    | any zero fs = int 0
    | otherwise = reOp OpMul $ biAssoc mul mul fs

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

