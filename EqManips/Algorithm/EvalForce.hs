module EqManips.Algorithm.EvalForce( floatEvalRules ) where


import Data.Maybe

import EqManips.Types
import EqManips.Propreties
import EqManips.EvaluationContext
import EqManips.Algorithm.Cleanup
import EqManips.Algorithm.Inject
import EqManips.Algorithm.Derivative
import EqManips.Algorithm.Utils
import EqManips.Algorithm.MetaEval

import EqManips.Algorithm.Unification

import Data.List( foldl' , transpose, sort )

left :: (Monad m) => a -> m (Either a b)
left = return . Left

right :: (Monad m) => b -> m (Either a b)
right = return . Right

-----------------------------------------------
----            '+'
-----------------------------------------------
add :: EvalOp
add (CInteger i1) (CFloat f2) = left . CFloat $ fromIntegral i1 + f2
add (CFloat f1) (CInteger i2) = left . CFloat $ f1 + fromIntegral i2
add (CFloat f1) (CFloat f2) = left . CFloat $ f1 + f2
add e e' = right (e, e')

-----------------------------------------------
----            '-'
-----------------------------------------------
sub :: EvalOp
sub (CInteger i1) (CFloat f2) = left . CFloat $ fromIntegral i1 - f2
sub (CFloat f1) (CInteger i2) = left . CFloat $ f1 - fromIntegral i2
sub (CFloat f1) (CFloat f2) = left . CFloat $ f1 - f2
sub e e' = right (e,e')

-----------------------------------------------
----            '*'
-----------------------------------------------
mul :: EvalOp
mul (CInteger i1) (CFloat f2) = left . CFloat $ fromIntegral i1 * f2
mul (CFloat f1) (CInteger i2) = left . CFloat $ f1 * fromIntegral i2
mul (CFloat f1) (CFloat f2) = left . CFloat $ f1 * f2
mul e e' = right (e, e')

-----------------------------------------------
----        '/'
-----------------------------------------------
-- | Handle the division operator. Nicely handle the case
-- of division by 0.
division :: EvalOp
division l@(CFloat _) (CInteger i2) = division l . CFloat $ toEnum i2
division (CInteger i) r@(CFloat _) = division (CFloat $ toEnum i) r
division (CFloat i1) (CFloat i2) = left . CFloat $ i1 / i2
division f1 f2 = right (f1, f2)

-----------------------------------------------
----        '^'
-----------------------------------------------
-- | yeah handle all the power operation.
power :: EvalOp
power l@(CFloat _) (CInteger i2) = power l . CFloat $ toEnum i2
power (CInteger i) r@(CFloat _) = power (CFloat $ toEnum i) r
power (CFloat i1) (CFloat i2) = return . Left . CFloat $ i1 ** i2
power f1 f2 = right (f1, f2)

-----------------------------------------------
----        'floor'
-----------------------------------------------
floorEval :: Formula -> EqContext Formula
floorEval i@(CInteger _) = return i
floorEval (CFloat f) = return . CInteger $ floor f
floorEval f = return $ UnOp OpFloor f

-----------------------------------------------
----        'frac'
-----------------------------------------------
fracEval :: Formula -> EqContext Formula
fracEval (CInteger _) = return $ CInteger 0
fracEval (CFloat f) = return . CFloat . snd $ (properFraction f :: (Int,Double))
fracEval f = return $ UnOp OpFrac f

-----------------------------------------------
----        'Ceil'
-----------------------------------------------
ceilEval :: Formula -> EqContext Formula
ceilEval i@(CInteger _) = return i
ceilEval (CFloat f) = return . CInteger $ ceiling f
ceilEval f = return $ UnOp OpCeil f

-----------------------------------------------
----        'negate'
-----------------------------------------------
fNegate :: Formula -> EqContext Formula
fNegate (CInteger i) = return . CInteger $ negate i
fNegate (CFloat f) = return . CFloat $ negate f
fNegate (UnOp OpNegate f) = return f
fNegate f = return $ negate f

-----------------------------------------------
----        'abs'
-----------------------------------------------
fAbs :: Formula -> EqContext Formula
fAbs (CInteger i) = return . CInteger $ abs i
fAbs (CFloat f) = return . CFloat $ abs f
fAbs f = return $ abs f

-----------------------------------------------
----        lalalal operators
-----------------------------------------------
binOp :: BinOperator -> [Formula] -> Formula
binOp _ [x] = x
binOp op lst = BinOp op lst

-- | Evaluate a binary operator
binEval :: BinOperator -> EvalOp -> EvalOp -> [Formula] -> EqContext Formula
binEval op f inv formulaList 
    | op `hasProp` Associativ && op `hasProp` Commutativ = do
#ifdef _DEBUG
        addTrace ("Sorting => ", BinOp op formulaList)
#endif
        biAssocM f inv (sort formulaList) >>= return . binOp op

    | otherwise = do
#ifdef _DEBUG
        addTrace ("Basic Eval=>", BinOp op formulaList)
#endif
        biAssocM f inv formulaList >>= return . binOp op

-----------------------------------------------
----        General evaluation
-----------------------------------------------
-- | General evaluation/reduction function
floatEvalRules :: Formula -> EqContext Formula
floatEvalRules (NumEntity Pi) = return $ CFloat pi
floatEvalRules (BinOp OpAdd fs) = binEval OpAdd add add fs
floatEvalRules (BinOp OpSub fs) = binEval OpSub sub add fs
floatEvalRules (BinOp OpMul fs) = binEval OpMul mul mul fs
-- | Todo fix this, it's incorrect
floatEvalRules (BinOp OpPow fs) = binEval OpPow power power fs
floatEvalRules (BinOp OpDiv fs) = binEval OpDiv division mul fs

floatEvalRules (UnOp OpFloor f) = floorEval f
floatEvalRules (UnOp OpCeil f) = ceilEval f
floatEvalRules (UnOp OpFrac f) = fracEval f

floatEvalRules (UnOp OpNegate f) = fNegate f
floatEvalRules (UnOp OpAbs f) = fAbs f

floatEvalRules (UnOp op f) = unOpReduce (funOf op) =<< eval f
    where funOf :: Floating a => UnOperator -> (a -> a)
          funOf OpSqrt = sqrt
          funOf OpSin = sin
          funOf OpSinh = sinh
          funOf OpASin = asin
          funOf OpASinh = asinh
          funOf OpCos = cos
          funOf OpCosh = cosh
          funOf OpACos = acos
          funOf OpACosh = acosh
          funOf OpTan = tan
          funOf OpTanh = tanh
          funOf OpATan = atan
          funOf OpATanh = atanh
          funOf OpLn = log
          funOf OpLog = \n -> log n / log 10.0
          funOf OpExp = exp
          funOf OpAbs = error $ Err.not_here "unop : abs - "
          funOf OpNegate = error $ Err.not_here "unop : negate - "
          funOf OpFloor = error $ Err.not_here "unop : floor - "
          funOf OpFrac =  error $ Err.not_here "unop : frac - "
          funOf OpCeil = error $ Err.not_here "unop : ceil - "
          funOf OpFactorial = error $ Err.not_here "unop : Should - "

floatEvalRules end = return end

--------------------------------------------------------------
---- Scalar related function
--------------------------------------------------------------
unOpReduce :: (forall a. (Floating a) => a -> a) -> Formula -> EqContext Formula
unOpReduce f (CInteger i) = unOpReduce f . CFloat $ toEnum i
unOpReduce f (CFloat num) = return . CFloat $ f num
unOpReduce f formula = return . f =<< eval formula

