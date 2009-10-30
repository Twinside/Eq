{-# LANGUAGE Rank2Types #-}
-- | This module implements the rules to interpret all floating
-- points operations which are by nature lossy. So this set
-- of rules may or may not be used in the context of global
-- evaluation to preserve the "true" meaning of the formula.
module EqManips.Algorithm.EvalFloating ( evalFloat, floatEvalRules ) where


import qualified EqManips.ErrorMessages as Err
import EqManips.Types
import EqManips.Propreties
import EqManips.EvaluationContext
import EqManips.Algorithm.Utils

import Data.Maybe( fromMaybe )
import Data.List( sort )

type EvalOp = FormulaPrim -> FormulaPrim -> EqContext (Either FormulaPrim (FormulaPrim,FormulaPrim))
type EvalFun = FormulaPrim -> EqContext FormulaPrim

-- | General function favored to use the reduction rules
-- as it preserve meta information about the formula form.
evalFloat :: Formula anyForm -> EqContext (Formula anyForm)
evalFloat (Formula f) = floatEvalRules f >>= (return . Formula)

left :: (Monad m) => a -> m (Either a b)
left = return . Left

right :: (Monad m) => b -> m (Either a b)
right = return . Right

floatCastingOperator :: (Double -> Double -> Double) -> EvalOp
floatCastingOperator f (CInteger i1) (CFloat f2) =
    left . CFloat $ f (fromIntegral i1) f2
floatCastingOperator f (CFloat f1) (CInteger i2) =
    left . CFloat $ f f1 (fromIntegral i2)
floatCastingOperator f (CFloat f1) (CFloat f2) =
    left . CFloat $ f f1 f2
floatCastingOperator _ e e' = right (e, e')

add, sub, mul, division, power :: EvalOp
add = floatCastingOperator (+)
sub = floatCastingOperator (-)
mul = floatCastingOperator (*)
division = floatCastingOperator (/)
power = floatCastingOperator (**)

-----------------------------------------------
----        'floor'
-----------------------------------------------
floorEval :: EvalFun
floorEval (CFloat f) = return . CInteger $ floor f
floorEval f = return $ UnOp OpFloor f

-----------------------------------------------
----        'frac'
-----------------------------------------------
fracEval :: EvalFun
fracEval (CFloat f) = return . CFloat . snd $ (properFraction f :: (Int,Double))
fracEval f = return $ UnOp OpFrac f

-----------------------------------------------
----        'Ceil'
-----------------------------------------------
ceilEval :: EvalFun
ceilEval i@(CInteger _) = return i
ceilEval (CFloat f) = return . CInteger $ ceiling f
ceilEval f = return $ UnOp OpCeil f

-----------------------------------------------
----        'negate'
-----------------------------------------------
fNegate :: EvalFun
fNegate (CFloat f) = return . CFloat $ negate f
fNegate f = return $ negate f

-----------------------------------------------
----        'abs'
-----------------------------------------------
fAbs :: EvalFun
fAbs (CFloat f) = return . CFloat $ abs f
fAbs f = return $ abs f

-----------------------------------------------
----        lalalal operators
-----------------------------------------------
binOp :: BinOperator -> [FormulaPrim] -> FormulaPrim
binOp _ [x] = x
binOp op lst = BinOp op lst

-- | Evaluate a binary operator
binEval :: BinOperator -> EvalOp -> EvalOp -> [FormulaPrim] -> EqContext FormulaPrim
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
-- | All the rules for floats
floatEvalRules :: FormulaPrim -> EqContext FormulaPrim
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

floatEvalRules formula@(UnOp op f) =
  return . fromMaybe formula $ unOpReduce (funOf op) f
    where funOf OpSqrt = sqrt
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
unOpReduce :: (forall a. (Floating a) => a -> a) -> FormulaPrim -> Maybe FormulaPrim
unOpReduce f (CInteger i) = unOpReduce f . CFloat $ (fromInteger i)
unOpReduce f (CFloat num) = Just . CFloat $ f num
unOpReduce _ _ = Nothing

