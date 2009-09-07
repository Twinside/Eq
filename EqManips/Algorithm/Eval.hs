{-# LANGUAGE Rank2Types #-}
module EqManips.Algorithm.Eval( reduce
                              , runProgramm
                              , evalGlobalStatement 
                              ) where

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

type FormulOperator = Formula -> Formula -> Formula
type EvalOp = Formula -> Formula -> EqContext (Either Formula (Formula,Formula))

-- | Main function to evaluate another function
reduce :: Formula -> EqContext Formula
reduce = eval

left :: (Monad m) => a -> m (Either a b)
left = return . Left

right :: (Monad m) => b -> m (Either a b)
right = return . Right

runProgramm :: [Formula] -> EqContext [Formula]
runProgramm = mapM evalGlobalStatement

-----------------------------------------------
----        Top level evaluation
-----------------------------------------------
-- | Add a function into the symbol table.
addLambda :: String -> [Formula] -> Formula -> EqContext ()
addLambda varName args body = do
    symb <- symbolLookup varName
    case symb of
      Nothing -> addSymbol varName $ Lambda [(args, body)]
      Just (Lambda clauses@((prevArg,_):_)) -> do
          if length prevArg /= length args
            then do
             eqFail (Variable varName) "Warning definition with different argument count"
             return ()
            else updateSymbol varName . Lambda $ clauses ++ [(args, body)]
          
      Just _ -> do
         eqFail (Variable varName) $ varName ++ " already defined as not a function"
         return ()

-- | Add a "value" into the symbol table
addVar :: String -> Formula -> EqContext ()
addVar varName body = do
    symb <- symbolLookup varName
    case symb of
      Nothing -> addSymbol varName body
      Just _ -> do
         eqFail (Variable varName) $ varName ++ " is already defined"
         return ()

-- | Evaluate top level declarations
evalGlobalStatement :: Formula -> EqContext Formula
evalGlobalStatement (BinOp OpEq [ (App (Variable funName) argList)
                                , body ]) = do
    addLambda funName argList body
    return $ (BinOp OpEq [(App (Variable funName) argList), body])

evalGlobalStatement (BinOp OpEq [(Variable varName), body]) = do
    pushContext
    body' <- reduce body
    popContext
    addVar varName body'
    return $ (BinOp OpEq [(Variable varName), body'])

evalGlobalStatement e = do
    pushContext
    a <- reduce e
    popContext
    return a

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
division l@(Matrix _ _ _) r@(Matrix _ _ _) = do
    eqFail (l / r) "Division is not defined for matrixes"
    left $ Block 1 1 1

division f1 f2@(CInteger 0) = do
    eqFail (f1 / f2) "This expression evaluate to 0, and is used in a division."
    left $ Block 1 1 1

division f1 f2@(CFloat 0) = do
    eqFail (f1 / f2) "This expression evaluate to 0, and is used in a division."
    left $ Block 1 1 1

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
power f1 (CInteger i2) | i2 < 0 = return . Left $ CInteger 1 / (f1 ** CInteger (-i2))
power (CInteger i1) (CInteger i2) = return . Left . CInteger $ i1 ^ i2
power f1 f2 = return . Right $ (f1, f2)

-----------------------------------------------
----        '!'
-----------------------------------------------
factorial :: Formula -> EqContext Formula
factorial f@(CFloat _) = eqFail f "Can't apply factorial to real number"
factorial (CInteger 0) = return $ CInteger 1
factorial f@(CInteger i) | i > 0 = return . CInteger $ product [1 .. i]
                         | otherwise = eqFail f "No factorial of negative numbers"
factorial f@(Matrix _ _ _) = eqFail f "No factorial of matrix"
factorial a = return $ UnOp OpFactorial a

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
fNegate f = return f

-----------------------------------------------
----        'abs'
-----------------------------------------------
fAbs :: Formula -> EqContext Formula
fAbs (CInteger i) = return . CInteger $ abs i
fAbs (CFloat f) = return . CFloat $ abs f
fAbs f = return f

-----------------------------------------------
----        'Comparison operators'
-----------------------------------------------
predicateList :: BinOperator 
              -> (Formula -> Formula -> Maybe Bool)
              -> [Formula]
              -> EqContext Formula
predicateList _ _ [] = error "predicate list - Operator denormalized"
predicateList _ _ [_] = error "predicate list - Operator denormalized"
predicateList op f (x:y:xs) = lastRez 
                            {-. lastCase -}
                            $ foldl' transform ([], False, x) (y:xs)
    where transform (acc@[Truth False],_,_) curr = (acc, False, curr)
          transform (acc, allWritten, prev) curr =
              case (f prev curr, allWritten) of
                   (Nothing, True)  -> (acc ++ [curr], True, curr)
                   (Nothing, False) -> (acc ++ [prev, curr], True, curr)
                   (Just True, _)   -> (acc, False, curr)
                   (Just False, _)  -> ([Truth False], True, curr)

          lastRez ([],_,_) = return $ Truth True
          lastRez ([e],_,_) = return e
          lastRez (lst,_,_) = return $ BinOp op lst

compOperator :: (forall a. Ord a => a -> a -> Bool)
             -> Formula -> Formula
             -> Maybe Bool
compOperator f (CInteger a) (CInteger b) = Just $ f a b
compOperator f (CFloat a) (CFloat b) = Just $ f a b
compOperator f a@(CFloat _) (CInteger b) =
    compOperator f a . CFloat $ fromIntegral b
compOperator f (CInteger a) b@(CFloat _) =
    compOperator f (CFloat $ fromIntegral a) b
compOperator _ _ _ = Nothing

-----------------------------------------------
----        AND
-----------------------------------------------
binand :: EvalOp
binand (Truth True) (Truth True) = return . Left $ Truth True
binand (Truth False) _ = return . Left $ Truth False
binand _ (Truth False) = return . Left $ Truth False
binand (Truth True) l = return . Left $ l
binand l (Truth True) = return . Left $ l
binand a b = return $ Right (a,b)

-----------------------------------------------
----        OR
-----------------------------------------------
binor :: EvalOp
binor (Truth False) (Truth False) = return . Left $ Truth False
binor (Truth True) _ = return . Left $ Truth True
binor _ (Truth True) = return . Left $ Truth True
binor (Truth False) l = return . Left $ l
binor l (Truth False) = return . Left $ l
binor a b = return $ Right (a,b)

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
        addTrace ("Sorting => ", BinOp op formulaList)
        biAssocM f inv (sort formulaList) >>= return . binOp op

    | otherwise = do
        addTrace ("Basic Eval=>", BinOp op formulaList)
        biAssocM f inv formulaList >>= return . binOp op

-----------------------------------------------
----        General evaluation
-----------------------------------------------
-- | General evaluation/reduction function
eval :: Formula -> EqContext Formula
eval (Meta m f) = metaEval m f
eval (NumEntity Pi) = return $ CFloat pi
eval (Matrix n m mlines) = do
    cells <- sequence [mapM eval line | line <- mlines]
    return $ Matrix n m cells

eval (Variable v) = symbolLookup v
    >>= return . fromMaybe (Variable v)

eval (App def var) = do
    redDef <- eval def
    redVar <- mapM eval var
    addTrace ("Appbegin |", App redDef redVar)
    needApply redDef redVar
   where needApply (Lambda funArgs) args' =
           case getFirstUnifying funArgs args' of
                Nothing -> eqFail (App def var) "Error can't apply function"
                Just (body, subst) -> do
                    pushContext
                    {-setContext subst-}
                    addSymbols subst
                    traceContext
                    addTrace ("subst | " ++ show subst, body)
                    body' <- eval body
                    addTrace ("body' | " ++ show body', body')
                    popContext
                    traceContext
                    return body'
         needApply def' args = do
             return $ App def' args

eval (BinOp OpAdd fs) = binEval OpAdd add add =<< mapM eval fs
eval (BinOp OpSub fs) = binEval OpSub sub add =<< mapM eval fs
eval (BinOp OpMul fs) = binEval OpMul mul mul =<< mapM eval fs
-- | Todo fix this, it's incorrect
eval (BinOp OpPow fs) = binEval OpPow power power =<< mapM eval fs
eval (BinOp OpDiv fs) = binEval OpDiv division mul =<< mapM eval fs

-- comparisons operators
eval (BinOp OpLt fs) = predicateList OpLt (compOperator (<)) =<< mapM eval fs
eval (BinOp OpGt fs) = predicateList OpGt (compOperator (>)) =<< mapM eval fs
eval (BinOp OpLe fs) = predicateList OpLe (compOperator (<=)) =<< mapM eval fs
eval (BinOp OpGe fs) = predicateList OpGe (compOperator (>=)) =<< mapM eval fs

{-eval (BinOp OpNe fs) = binEval OpNe (compOperator (/=)) =<< mapM eval fs-}

eval (BinOp OpEq [v@(Variable _),f2]) = do
    f2' <- eval f2
    return $ BinOp OpEq [v,f2']

eval (BinOp OpAnd fs) = binEval OpAnd binand binand =<< mapM eval fs
eval (BinOp OpOr fs) = binEval OpOr binor binor =<< mapM eval fs

eval (UnOp OpFactorial f) = factorial =<< eval f
eval (UnOp OpFloor f) = floorEval =<< eval f
eval (UnOp OpCeil f) = ceilEval =<< eval f
eval (UnOp OpFrac f) = fracEval =<< eval f

eval (UnOp OpNegate f) = fNegate =<< eval f
eval (UnOp OpAbs f) = fAbs =<< eval f

eval (UnOp op f) = unOpReduce (funOf op) =<< eval f
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
          funOf OpAbs = error "abs - unop - shouldn't happen here"
          funOf OpNegate = error "negate - unop - shouldn't happen here"
          funOf OpFloor = error "floor - unop - should not happen here"
          funOf OpFrac =  error "frac - unop - should not happen here"
          funOf OpCeil = error "ceil - unop - should not happen here"
          funOf OpFactorial = error "Should not happen here"

eval (Derivate what (Meta op var)) = do
    evalued <- metaEval op var
    eval $ Derivate what evalued

eval (Derivate f@(Meta op _) var) = do
    evalued <- metaEval op f
    eval (Derivate evalued var)

eval (Derivate what (Variable s)) = do
    addTrace ("Derivation on " ++ s, what)
    derived <- derivate what s
    return $ cleanup derived

eval f@(Derivate _ _) =
    eqFail f "Sorry your derivation doesn't have a good variable specification"

eval formu@(Sum (BinOp OpEq [Variable v, CInteger initi])
                (CInteger endi)
                f)
     | initi <= endi = iterateFormula (BinOp OpAdd) v initi endi f
     | otherwise = eqFail formu "Sorry, your sum as wrong bounds, can't evaluate"

eval formu@(Product (BinOp OpEq [Variable v, CInteger initi])
                    (CInteger endi)
                    f)
     | initi <= endi = iterateFormula (BinOp OpMul) v initi endi f
     | otherwise = eqFail formu "Sorry, your product as wrong bounds, can't evaluate"

eval f@(Integrate _ _ _ _) =
    eqFail f "No algorithm to integrate your function, sorry"

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
unOpReduce :: (forall a. (Floating a) => a -> a) -> Formula -> EqContext Formula
unOpReduce f (CInteger i) = unOpReduce f . CFloat $ toEnum i
unOpReduce f (CFloat num) = return . CFloat $ f num
unOpReduce f formula = return . f =<< eval formula

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

