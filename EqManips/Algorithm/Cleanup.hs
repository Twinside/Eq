module EqManips.Algorithm.Cleanup ( cleanup, cleanupRules ) where

import EqManips.Types
import EqManips.Polynome
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
divide f1@(CInteger i1) f2@(CInteger i2)
    | i1 `mod` i2 == 0 = Left . int $ i1 `div` i2
    | otherwise = if greatestCommonDenominator > 1
                        then Left $ (int $ i1 `quot` greatestCommonDenominator)
                                  / (int $ i2 `quot` greatestCommonDenominator)
                        else Right (f1,f2)
        where greatestCommonDenominator = gcd i1 i2
divide x y = Right (x,y)

----------------------------------------------
----                'sinus'
----------------------------------------------
sinus :: FormulaPrim -> FormulaPrim
sinus (CInteger 0) = int 0
sinus (NumEntity Pi) = int 0
sinus (BinOp OpDiv [NumEntity Pi, CInteger 6]) = int 1 / int 2
sinus (BinOp OpMul [NumEntity Pi, CInteger _]) = int 0
sinus (BinOp OpMul [CInteger _, NumEntity Pi]) = int 0
-- TODO : add more complex simplifications one day :]
{-sinus (BinOp OpMul [Pi, BinOp OpDiv [Pi, CInteger i]])-}
sinus i = sin i

----------------------------------------------
----                'cosinus'
----------------------------------------------
cosinus :: FormulaPrim -> FormulaPrim
cosinus (CInteger 0) = int 1
cosinus (NumEntity Pi) = int (-1)
cosinus (BinOp OpDiv [NumEntity Pi, CInteger 6]) = sqrt 3 / int 3
cosinus (BinOp OpMul [NumEntity Pi, CInteger i])
    | i `mod` 2 == 0 = int 1
    | otherwise = int (-1)
cosinus (BinOp OpMul [CInteger i, NumEntity Pi])
    | i `mod` 2 == 0 = int 1
    | otherwise = int (-1)
cosinus i = cos i

--------------------------------------------------
----            'tan'
--------------------------------------------------
tangeant :: FormulaPrim -> FormulaPrim
tangeant (BinOp OpDiv [NumEntity Pi, CInteger 4]) = int 1
tangeant i = tan i

--------------------------------------------------
----            'asinh'
--------------------------------------------------
sinush :: FormulaPrim -> FormulaPrim
sinush (CInteger 0) = int 0
sinush (UnOp OpNegate x) = negate $ sinh x
sinush (CFloat f)   | f < 0 = negate . sinh $ CFloat (-f)
sinush (CInteger i) | i < 0 = negate . sinh $ CInteger (-i)
sinush i = sinh i

--------------------------------------------------
----            'cosinush'
--------------------------------------------------
cosinush :: FormulaPrim -> FormulaPrim
cosinush (CInteger 0) = int 0
cosinush (UnOp OpNegate x) = cosh x
cosinush (CFloat f)   | f < 0 = cosh $ CFloat (-f)
cosinush (CInteger i) | i < 0 = cosh $ CInteger (-i)
cosinush i = cosh i

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

polyclean :: Polynome -> FormulaPrim
polyclean p = resulter $ pclean p
    where pclean (Polynome var lst) = packPoly . Polynome var $ foldr reducer [] lst
          pclean rest@(PolyRest _) = rest

          reducer (  _, PolyRest r) acc | isCoeffNull r = acc
          reducer (deg, p'@(Polynome _ _)) acc = (deg, pclean p') : acc
          reducer a acc = a : acc

          packPoly (Polynome _ [(deg, rest@(PolyRest _))]) | isCoeffNull deg = rest
          packPoly a = a

          resulter (PolyRest c) = coefToFormula c
          resulter (Polynome _ [(deg, PolyRest c)]) | isCoeffNull deg = coefToFormula c
          resulter l = Poly l

---------------------------------------------
---- Linking all the rules together
---------------------------------------------
rules :: FormulaPrim -> FormulaPrim
rules (Poly (PolyRest r)) = coefToFormula r
rules (Poly p) = polyclean p
rules (UnOp OpSin f) = sinus f
rules (UnOp OpCos f) = cosinus f
rules (UnOp OpTan f) = tangeant f
rules (UnOp OpSinh f) = sinush f
rules (UnOp OpCosh f) = cosinush f
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

