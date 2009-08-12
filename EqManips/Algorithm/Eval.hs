{-# LANGUAGE Rank2Types #-}
module EqManips.Algorithm.Eval( reduce ) where

import EqManips.Types
import EqManips.EvaluationContext
import EqManips.Algorithm.Cleanup
import EqManips.Algorithm.Inject
import EqManips.Algorithm.Derivative
import EqManips.Algorithm.Utils

import Data.List( foldl' , transpose )

type FormulOperator = Formula -> Formula -> Formula
type EvalOp = Formula -> Formula -> EqContext (Either Formula (Formula,Formula))

-- | Main function to evaluate another function
reduce :: Formula -> EqContext Formula
reduce = eval

left :: (Monad m) => a -> m (Either a b)
left = return . Left

right :: (Monad m) => b -> m (Either a b)
right = return . Right

-----------------------------------------------
----            '+'
-----------------------------------------------
add :: EvalOp
add (CInteger i1) (CInteger i2) = left . CInteger $ i1 + i2
add (CInteger i1) (CFloat f2) = left . CFloat $ fromIntegral i1 + f2
add (CFloat f1) (CInteger i2) = left . CFloat $ f1 + fromIntegral i2
add (CFloat f1) (CFloat f2) = left . CFloat $ f1 + f2
add f1@(Matrix _ _ _) f2@(Matrix _ _ _) =
    matrixMatrixSimple (+) f1 f2
add f1@(Matrix _ _ _) f2 = do
    eqFail (f1+f2) "Error invalid addition on Matrix"
    right (f1, f2)
add f1 f2@(Matrix _ _ _) = do
    eqFail (f1+f2) "Error invalid addition on Matrix"
    right (f1, f2)

add e e' = right (e, e')

-----------------------------------------------
----            '-'
-----------------------------------------------
sub :: EvalOp
sub (CInteger i1) (CInteger i2) = left . CInteger $ i1 - i2
sub (CInteger i1) (CFloat f2) = left . CFloat $ fromIntegral i1 - f2
sub (CFloat f1) (CInteger i2) = left . CFloat $ f1 - fromIntegral i2
sub (CFloat f1) (CFloat f2) = left . CFloat $ f1 - f2
sub f1@(Matrix _ _ _) f2@(Matrix _ _ _) =
    matrixMatrixSimple (-) f1 f2
sub f1@(Matrix _ _ _) f2 = do
    eqFail (f1-f2) "Error invalid substraction on Matrix"
    right (f1, f2)
sub f1 f2@(Matrix _ _ _) = do
    eqFail (f1-f2) "Error invalid substraction on Matrix"
    right (f1, f2)
sub e e' = right (e,e')

-----------------------------------------------
----            '*'
-----------------------------------------------
mul :: EvalOp
mul (CInteger i1) (CInteger i2) = left . CInteger $ i1 * i2
mul (CInteger i1) (CFloat f2) = left . CFloat $ fromIntegral i1 * f2
mul (CFloat f1) (CInteger i2) = left . CFloat $ f1 * fromIntegral i2
mul (CFloat f1) (CFloat f2) = left . CFloat $ f1 * f2
mul f1@(Matrix _ _ _) f2@(Matrix _ _ _) = matrixMatrixMul f1 f2
mul m@(Matrix _ _ _) s = matrixScalar (*) m s >>= left
mul s m@(Matrix _ _ _) = matrixScalar (*) m s >>= left
mul e e' = right (e, e')

-----------------------------------------------
----        '/'
-----------------------------------------------
-- | Handle the division operator. Nicely handle the case
-- of division by 0.
division :: EvalOp
division f1 f2@(CInteger 0) = do
    eqFail (f1 / f2) "This expression evaluate to 0, and is used in a division."
    right (f1, f2)
division f1 f2@(CFloat 0) = do
    eqFail (f1 / f2) "This expression evaluate to 0, and is used in a division."
    right (f1, f2)

division l@(CFloat _) (CInteger i2) = division l . CFloat $ toEnum i2
division (CInteger i) r@(CFloat _) = division (CFloat $ toEnum i) r
division (CFloat i1) (CFloat i2) = left . CFloat $ i1 / i2
division (CInteger i1) (CInteger i2)
    | i1 `mod` i2 == 0 = left . CInteger $ i1 `div` i2
    | otherwise = left . CFloat $ toEnum i1 / toEnum i2
division m@(Matrix _ _ _) s = matrixScalar (/) m s >>= left
division s m@(Matrix _ _ _) = matrixScalar (/) m s >>= left
division f1 f2 = right (f1, f2)

-----------------------------------------------
----        '^'
-----------------------------------------------
-- | yeah handle all the power operation.
power :: EvalOp
power l@(CFloat _) (CInteger i2) = power l . CFloat $ toEnum i2
power (CInteger i) r@(CFloat _) = power (CFloat $ toEnum i) r
power (CFloat i1) (CFloat i2) = return . Left . CFloat $ i1 ** i2
power (CInteger i1) (CInteger i2) = return . Left . CInteger $ i1 ^ i2
power f1 (CInteger i2) | i2 < 0 = return . Left $ CInteger 1 / (f1 ** CInteger (-i2))
power f1 f2 = return . Right $ (f1, f2)

-- | Evaluate a binary operator
binEval :: BinOperator -> EvalOp -> EvalOp -> [Formula] -> EqContext Formula
binEval op f inv formulaList = biAssocM f inv formulaList >>= rez
    where rez [x] = return x
          rez lst = return $ BinOp op lst

-- | General evaluation/reduction function
eval :: Formula -> EqContext Formula
eval (Meta Force f) = eval f
eval (Meta Hold f) = return f
eval (Meta Listify f) = return $ listifyBinOp f
eval (Meta Treefy f) = return $ treeIfyBinOp f

eval (NumEntity Pi) = return $ CFloat pi
eval (Matrix n m mlines) = do
    cells <- sequence [mapM eval line | line <- mlines]
    return $ Matrix n m cells

eval (BinOp OpAdd fs) = binEval OpAdd add add =<< mapM eval fs
eval (BinOp OpSub fs) = binEval OpSub sub add =<< mapM eval fs
eval (BinOp OpMul fs) = binEval OpMul mul mul =<< mapM eval fs
-- | Todo fix this, it's incorrect
eval (BinOp OpPow fs) = binEval OpPow power power =<< mapM eval fs
eval (BinOp OpDiv fs) = binEval OpDiv division mul =<< mapM eval fs

eval (BinOp OpEq [v@(Variable _),f2]) = do
    f2' <- eval f2
    return $ BinOp OpEq [v,f2']

eval (UnOp op f) = unOpReduce (funOf op) f
    where funOf OpNegate = negate
          funOf OpAbs = abs
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

eval (Derivate what (Variable s)) =
    derivate what s >>= return . cleanup

eval f@(Derivate _ _) =
    eqFail f "Sorry your derivation doesn't have a good variable specification"

eval formu@(Sum (BinOp OpEq [Variable v, CInteger initi])
                (CInteger endi) 
                f) 
     | initi <= endi = iterateFormula (BinOp OpAdd) v initi endi f
     | otherwise = eqFail formu "Sorry, your sum as wrong bounds, can't evaluate"

eval f@(Sum _ _ _) = eqFail f "Sorry, your sum don't have the good form to be evaluated."

eval formu@(Product (BinOp OpEq [Variable v, CInteger initi])
                    (CInteger endi) 
                    f)
     | initi <= endi = iterateFormula (BinOp OpMul) v initi endi f
     | otherwise = eqFail formu "Sorry, your product as wrong bounds, can't evaluate"

eval f@(Product _ _ _) = eqFail f "Sorry, your product don't have the good form to be evaluated."

eval f@(Integrate _ _ _ _) =
    eqFail f "No algorithm to integrate your function, sorry"

eval (App def var) = do
    redDef <- eval def
    redVar <- mapM eval var
    return $ App redDef redVar

eval f@(Block _ _ _) = eqFail f "Block cannot be evaluated"
eval end = return end

--------------------------------------------------------------
---- iteration
--------------------------------------------------------------
iterateFormula :: ([Formula] -> Formula) -> String -> Int -> Int -> Formula
               -> EqContext Formula
iterateFormula op ivar initi endi what = do
    pushContext
    rez <- mapM combiner [initi .. endi]
    popContext
    case rez of
         [x] -> eval x
         _  -> eval $ op rez
     where combiner i = do
               addSymbol ivar (CInteger i)
               inject what


--------------------------------------------------------------
---- Scalar related function
--------------------------------------------------------------
unOpReduce :: (Double -> Double) -> Formula -> EqContext Formula
unOpReduce f (CInteger i) = unOpReduce f . CFloat $ toEnum i
unOpReduce f (CFloat num) = return . CFloat $ f num
unOpReduce _ formula = eval formula

--------------------------------------------------------------
---- Matrix related functions
--------------------------------------------------------------
matrixScalar :: (Formula -> Formula -> Formula) -> Formula -> Formula 
             -> EqContext Formula
matrixScalar op s m@(Matrix _ _ _) = matrixScalar op m s
matrixScalar op (Matrix n m mlines) s = cell >>= return . Matrix n m
    where cell = sequence
            [ mapM (\c -> eval $ c `op` s) line | line <- mlines]
matrixScalar _ _ _ = error "matrixScalar - Should be impossible"

-- | Multiplication between two matrix. Check for matrix sizes.
matrixMatrixMul :: EvalOp
matrixMatrixMul m1@(Matrix n _ mlines) m2@(Matrix n' m' mlines')
    | n /= m' = do eqFail (BinOp OpMul [m1, m2])
                       "Error can't multiply matrix, m2 has wrong height"
                   right (m1, m2)
    | otherwise = cellLine >>= left . Matrix n n'
        where cellLine = sequence
                    [ sequence [multCell $ zip line row | row <- transpose mlines' ]
                                                        | line <- mlines]

              multCell l = eval $ foldl' multAtor (initCase l) (tail l)
              multAtor acc (l, r) = acc + (l * r)

              initCase ((x,y):_) = x * y
              initCase _ = error "Should never happen : matrix are empty"
              
matrixMatrixMul _ _ = error "matrixMatrixMul - Shouldn't happen"

-- | Simple operation, matrix addition or substraction
matrixMatrixSimple :: FormulOperator -> Formula -> Formula 
                   -> EqContext (Either Formula (Formula,Formula))
matrixMatrixSimple op m1@(Matrix n m mlines) m2@(Matrix n' m' mlines')
    | n /= n' || m /= m' = do
        eqFail (m1 `op` m2) "Sorry can't apply this operation on matrix of different sizes"
        return $ Right (m1, m2)
    | otherwise = newCells >>= return . Left . Matrix n m
        where dop (e1, e2) = eval $ e1 `op`e2
              newCells = sequence [ mapM dop $ zip line1 line2
                                     | (line1, line2) <- zip mlines mlines']
matrixMatrixSimple _ _ _ = error "matrixMatrixSimple - Shouldn't happen"

