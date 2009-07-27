{-# LANGUAGE Rank2Types #-}
module EqManips.Algorithm.Eval where

import EqManips.Types
import EqManips.EvaluationContext
import EqManips.Algorithm.Cleanup
import EqManips.Algorithm.Derivative

import Data.List( foldl' , transpose )

type FormulOperator = Formula -> Formula -> Formula

-- General reduction
reduce :: Formula -> EqContext Formula
reduce (Matrix n m mlines) = do
    cells <- sequence [mapM reduce line | line <- mlines]
    return $ Matrix n m cells

-- All valid Matrix/Matrix operations
reduce (BinOp OpSub f1@(Matrix _ _ _) f2@(Matrix _ _ _)) =
    matrixMatrixSimple (-) f1 f2
reduce (BinOp OpAdd f1@(Matrix _ _ _) f2@(Matrix _ _ _)) =
    matrixMatrixSimple (+) f1 f2
reduce (BinOp OpMul f1@(Matrix _ _ _) f2@(Matrix _ _ _)) =
    matrixMatrixMul f1 f2

-- All valid Matrix/Scalar operations
reduce (BinOp OpMul m@(Matrix _ _ _) s) =matrixScalar (*) m s
reduce (BinOp OpMul s m@(Matrix _ _ _)) = matrixScalar (*) m s
reduce (BinOp OpDiv m@(Matrix _ _ _) s) = matrixScalar (/) m s
reduce (BinOp OpDiv s m@(Matrix _ _ _)) = matrixScalar (/) m s

-- Everything else covering Matrix is Bullshit
reduce f@(BinOp _ (Matrix _ _ _) _) = eqFail f "Error invalid operation on Matrix"
reduce f@(BinOp _ _ (Matrix _ _ _)) = eqFail f "Error invalid operation on Matrix"

reduce (BinOp OpAdd f1 f2) = binOpReduce (+) f1 f2
reduce (BinOp OpSub f1 f2) = binOpReduce (-) f1 f2
reduce (BinOp OpMul f1 f2) = binOpReduce (*) f1 f2

{-reduce (NumEntity Pi) = return $ CFloat pi-}
{-reduce (UnOp UnOperator Formula)-}

reduce (BinOp OpDiv f1 f2) = division f1 f2
{-reduce (BinOp OpPow f1 f2) =-}

reduce (Derivate what (Variable s)) =
    derivate what s >>= cleanup

reduce f@(Derivate _ _) =
    eqFail f "Sorry your derivation doesn't have a good variable specification"

reduce f@(Sum _ _ _) = eqFail f "Sorry, sum is not implemented yet"
reduce f@(Product _ _ _) = eqFail f "Sorry, product is not implemented yet"

reduce f@(App _ _) =
    eqFail f "Sorry, no algorithm for your function yet"

reduce f@(Integrate _ _ _ _) =
    eqFail f "No algorithm to integrate your function, sorry"

reduce f@(Block _ _ _) =
    eqFail f "Block cannot be evaluated"

reduce end = return end

--------------------------------------------------------------
---- Scalar related function
--------------------------------------------------------------
binOpReduce :: (forall a. (Num a) => (a -> a -> a)) 
            -> Formula -> Formula -> EqContext Formula
binOpReduce op f1 f2 = do
    f1' <- reduce f1
    f2' <- reduce f2
    return $ simply op f1' f2'

simply :: (forall a. (Num a) => (a -> a -> a))
       -> Formula -> Formula -> Formula
simply op (CInteger i1) (CInteger i2) = CInteger $ i1 `op` i2
simply op (CInteger i1) (CFloat f2) = CFloat $ fromIntegral i1 `op` f2
simply op (CFloat f1) (CInteger i2) = CFloat $ f1 `op` fromIntegral i2
simply op (CFloat f1) (CFloat f2) = CFloat $ f1 `op` f2
simply op e e' = e `op` e'

division :: Formula -> Formula -> EqContext Formula
division f1 f2 = return $ f1 / f2

--------------------------------------------------------------
---- Matrix related functions
--------------------------------------------------------------
matrixScalar :: (Formula -> Formula -> Formula) -> Formula -> Formula 
             -> EqContext Formula
matrixScalar op s m@(Matrix _ _ _) = matrixScalar op m s
matrixScalar op (Matrix n m mlines) s = cell >>= return . Matrix n m
    where cell = sequence
            [ mapM (\c -> reduce $ c `op` s) line | line <- mlines]
matrixScalar _ _ _ = error "matrixScalar - Should be impossible"

-- | Multiplication between two matrix. Check for matrix sizes.
matrixMatrixMul :: Formula -> Formula -> EqContext Formula
matrixMatrixMul m1@(Matrix n _ mlines) m2@(Matrix n' m' mlines')
    | n /= m' = eqFail (BinOp OpMul m1 m2)
                       "Error can't multiply matrix, m2 has wrong height"
    | otherwise = cellLine >>= return . Matrix n n'
        where cellLine = sequence
                    [ sequence [multCell $ zip line row | row <- transpose mlines' ]
                                                        | line <- mlines]

              multCell l = reduce $ foldl' multAtor (initCase l) (tail l)
              multAtor acc (l, r) = acc + (l * r)

              initCase ((x,y):_) = x * y
              initCase _ = error "Should never happen : matrix are empty"
              
matrixMatrixMul _ _ = error "matrixMatrixMul - Shouldn't happen"

-- | Simple operation, matrix addition or substraction
matrixMatrixSimple :: FormulOperator -> Formula -> Formula -> EqContext Formula
matrixMatrixSimple op m1@(Matrix n m mlines) m2@(Matrix n' m' mlines')
    | n /= n' || m /= m' = eqFail (m1 `op` m2)
                                  "Sorry can't apply this operation on matrix of different sizes"
    | otherwise = newCells >>= return . Matrix n m
        where dop (e1, e2) = reduce $ e1 `op`e2
              newCells = sequence [ mapM dop $ zip line1 line2
                                     | (line1, line2) <- zip mlines mlines']
matrixMatrixSimple _ _ _ = error "matrixMatrixSimple - Shouldn't happen"

