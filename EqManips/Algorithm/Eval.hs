{-# LANGUAGE Rank2Types #-}
module EqManips.Algorithm.Eval( reduce
                              , runProgramm
                              , evalGlobalStatement 
                              ) where

import Data.Maybe

import qualified EqManips.ErrorMessages as Err

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
reduce = eval (eval return) . cleanup

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
             eqFail (Variable varName) Err.def_diff_argcount
             return ()
            else updateSymbol varName . Lambda $ clauses ++ [(args, body)]
          
      Just _ -> do
         eqFail (Variable varName) $ Err.def_not_lambda varName
         return ()

-- | Add a "value" into the symbol table
addVar :: String -> Formula -> EqContext ()
addVar varName body = do
    symb <- symbolLookup varName
    case symb of
      Nothing -> addSymbol varName body
      Just _ -> do
         eqFail (Variable varName) $ Err.def_already varName
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
add f1@(Matrix _ _ _) f2@(Matrix _ _ _) =
    matrixMatrixSimple (+) f1 f2
add f1@(Matrix _ _ _) f2 = do
    eqFail (f1+f2) Err.add_matrix
    right (f1, f2)
add f1 f2@(Matrix _ _ _) = do
    eqFail (f1+f2) Err.add_matrix
    right (f1, f2)

add e e' = right (e, e')

-----------------------------------------------
----            '-'
-----------------------------------------------
sub :: EvalOp
sub (CInteger i1) (CInteger i2) = left . CInteger $ i1 - i2
sub f1@(Matrix _ _ _) f2@(Matrix _ _ _) =
    matrixMatrixSimple (-) f1 f2
sub f1@(Matrix _ _ _) f2 = do
    eqFail (f1-f2) Err.sub_matrix
    right (f1, f2)
sub f1 f2@(Matrix _ _ _) = do
    eqFail (f1-f2) Err.sub_matrix
    right (f1, f2)
sub e e' = right (e,e')

-----------------------------------------------
----            '*'
-----------------------------------------------
mul :: EvalOp
mul (CInteger i1) (CInteger i2) = left . CInteger $ i1 * i2
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
    eqFail (l / r) Err.div_undefined_matrixes
    left $ Block 1 1 1

division f1 f2@(CInteger 0) = do
    eqFail (f1 / f2) Err.div_by_0
    left $ Block 1 1 1

division f1 f2@(CFloat 0) = do
    eqFail (f1 / f2) Err.div_by_0
    left $ Block 1 1 1

division (CInteger i1) (CInteger i2)
    | i1 `mod` i2 == 0 = left . CInteger $ i1 `div` i2
division m@(Matrix _ _ _) s = matrixScalar (/) m s >>= left
division s m@(Matrix _ _ _) = matrixScalar (/) m s >>= left
division f1 f2 = right (f1, f2)

-----------------------------------------------
----        '^'
-----------------------------------------------
-- | yeah handle all the power operation.
power :: EvalOp
power f1 (CInteger i2) | i2 < 0 = return . Left $ CInteger 1 / (f1 ** CInteger (-i2))
power (CInteger i1) (CInteger i2) = return . Left . CInteger $ i1 ^ i2
power f1 f2 = return . Right $ (f1, f2)

-----------------------------------------------
----        '!'
-----------------------------------------------
factorial :: Formula -> EqContext Formula
factorial f@(CFloat _) = eqFail f Err.factorial_on_real 
factorial (CInteger 0) = return $ CInteger 1
factorial f@(CInteger i) | i > 0 = return . CInteger $ product [1 .. i]
                         | otherwise = eqFail f Err.factorial_negative
factorial f@(Matrix _ _ _) = eqFail f Err.factorial_matrix
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
----        'Comparison operators'
-----------------------------------------------
predicateList :: BinOperator 
              -> (Formula -> Formula -> Maybe Bool)
              -> [Formula]
              -> EqContext Formula
predicateList _ _ [] = error $ Err.empty_binop "predicate list - "
predicateList _ _ [_] = error $ Err.single_binop "predicate list - "
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
eval :: (Formula -> EqContext Formula)
     -> Formula -> EqContext Formula
eval evaluator (Meta m f) = metaEval m f
eval evaluator (NumEntity Pi) = return $ CFloat pi
eval evaluator (Matrix n m mlines) = do
    cells <- sequence [mapM evaluator line | line <- mlines]
    return $ Matrix n m cells

eval evaluator (Variable v) = symbolLookup v
    >>= return . fromMaybe (Variable v)

eval evaluator (App def var) = do
    redDef <- evaluator def
    redVar <- mapM evaluator var
#ifdef _DEBUG
    addTrace ("Appbegin |", App redDef redVar)
#endif
    needApply redDef redVar
   where needApply (Lambda funArgs) args' =
           case getFirstUnifying funArgs args' of
                Nothing -> eqFail (App def var) Err.app_no_applygindef
                Just (body, subst) -> do
                    pushContext
                    addSymbols subst
#ifdef _DEBUG
                    {-traceContext-}
                    addTrace ("subst | " ++ show subst, body)
#endif
                    body' <- evaluator body
#ifdef _DEBUG
                    addTrace ("body' | " ++ show body', body')
#endif
                    popContext
                    return body'
         needApply def' args = do
             return $ App def' args

eval evaluator (BinOp OpAdd fs) = binEval OpAdd add add =<< mapM evaluator fs
eval evaluator (BinOp OpSub fs) = binEval OpSub sub add =<< mapM evaluator fs
eval evaluator (BinOp OpMul fs) = binEval OpMul mul mul =<< mapM evaluator fs
-- | Todo fix this, it's incorrect
eval evaluator (BinOp OpPow fs) = binEval OpPow power power =<< mapM evaluator fs
eval evaluator (BinOp OpDiv fs) = binEval OpDiv division mul =<< mapM evaluator fs

-- comparisons operators
eval evaluator (BinOp OpLt fs) = predicateList OpLt (compOperator (<)) =<< mapM evaluator fs
eval evaluator (BinOp OpGt fs) = predicateList OpGt (compOperator (>)) =<< mapM evaluator fs
eval evaluator (BinOp OpLe fs) = predicateList OpLe (compOperator (<=)) =<< mapM evaluator fs
eval evaluator (BinOp OpGe fs) = predicateList OpGe (compOperator (>=)) =<< mapM evaluator fs

{-evaluator (BinOp OpNe fs) = binEval OpNe (compOperator (/=)) =<< mapM evaluator fs-}

eval evaluator (BinOp OpEq [v@(Variable _),f2]) = do
    f2' <- evaluator f2
    return $ BinOp OpEq [v,f2']

eval evaluator (BinOp OpAnd fs) = binEval OpAnd binand binand =<< mapM evaluator fs
eval evaluator (BinOp OpOr fs) = binEval OpOr binor binor =<< mapM evaluator fs

eval evaluator (UnOp OpFactorial f) = factorial =<< evaluator f
eval evaluator (UnOp OpFloor f) = floorEval =<< evaluator f
eval evaluator (UnOp OpCeil f) = ceilEval =<< evaluator f
eval evaluator (UnOp OpFrac f) = fracEval =<< evaluator f

eval evaluator (UnOp OpNegate f) = fNegate =<< evaluator f
eval evaluator (UnOp OpAbs f) = fAbs =<< evaluator f

eval evaluator (UnOp op f) = return . UnOp op =<< evaluator f

eval evaluator (Derivate what (Meta op var)) = do
    evalued <- metaEval op var
    evaluator $ Derivate what evalued

eval evaluator (Derivate f@(Meta op _) var) = do
    evalued <- metaEval op f
    evaluator (Derivate evalued var)

eval evaluator (Derivate what (Variable s)) = do
#ifdef _DEBUG
    addTrace ("Derivation on " ++ s, what)
#endif
    derived <- derivate what s
    return $ cleanup derived

eval evaluator f@(Derivate _ _) =
    eqFail f Err.deriv_bad_var_spec 

eval evaluator formu@(Sum (BinOp OpEq [Variable v, CInteger initi])
                (CInteger endi)
                f)
     | initi <= endi = iterateFormula evaluator (BinOp OpAdd) v initi endi f
     | otherwise = eqFail formu Err.sum_wrong_bounds

eval evaluator formu@(Product (BinOp OpEq [Variable v, CInteger initi])
                    (CInteger endi)
                    f)
     | initi <= endi = iterateFormula evaluator (BinOp OpMul) v initi endi f
     | otherwise = eqFail formu Err.product_wrong_bounds 

eval evaluator f@(Integrate _ _ _ _) =
    eqFail f Err.integration_no_eval

eval evaluator f@(Block _ _ _) = eqFail f Err.block_eval
eval evaluator end = return end

--------------------------------------------------------------
---- iteration
--------------------------------------------------------------
iterateFormula :: (Formula -> EqContext Formula)
               -> ([Formula] -> Formula) -> String -> Int -> Int -> Formula
               -> EqContext Formula
iterateFormula evaluator op ivar initi endi what = do
    pushContext
    rez <- mapM combiner [initi .. endi]
    popContext
    case rez of
         [x] -> evaluator x
         _  -> evaluator $ op rez
     where combiner i = do
               addSymbol ivar (CInteger i)
               inject what

--------------------------------------------------------------
---- Matrix related functions
--------------------------------------------------------------
matrixScalar :: (Formula -> Formula -> Formula) -> Formula -> Formula 
             -> EqContext Formula
matrixScalar op s m@(Matrix _ _ _) = matrixScalar op m s
matrixScalar op (Matrix n m mlines) s = cell >>= return . Matrix n m
    where cell = sequence
            [ mapM (\c -> eval $ c `op` s) line | line <- mlines]
matrixScalar _ _ _ = error Err.matrixScalar_badop

-- | Multiplication between two matrix. Check for matrix sizes.
matrixMatrixMul :: EvalOp
matrixMatrixMul m1@(Matrix n _ mlines) m2@(Matrix n' m' mlines')
    | n /= m' = do eqFail (BinOp OpMul [m1, m2]) Err.matrix_mul_bad_size
                   right (m1, m2)
    | otherwise = cellLine >>= left . Matrix n n'
        where cellLine = sequence
                    [ sequence [multCell $ zip line row | row <- transpose mlines' ]
                                                        | line <- mlines]

              multCell l = eval $ foldl' multAtor (initCase l) (tail l)
              multAtor acc (l, r) = acc + (l * r)

              initCase ((x,y):_) = x * y
              initCase _ = error . Err.shouldnt_happen $ Err.matrix_empty ++ " - "
              
matrixMatrixMul _ _ = error $ Err.shouldnt_happen "matrixMatrixMul - "

-- | Simple operation, matrix addition or substraction
matrixMatrixSimple :: FormulOperator -> Formula -> Formula 
                   -> EqContext (Either Formula (Formula,Formula))
matrixMatrixSimple op m1@(Matrix n m mlines) m2@(Matrix n' m' mlines')
    | n /= n' || m /= m' = do
        eqFail (m1 `op` m2) Err.matrix_diff_size
        return $ Right (m1, m2)
    | otherwise = newCells >>= return . Left . Matrix n m
        where dop (e1, e2) = eval $ e1 `op`e2
              newCells = sequence [ mapM dop $ zip line1 line2
                                     | (line1, line2) <- zip mlines mlines']
matrixMatrixSimple _ _ _ = error $ Err.shouldnt_happen "matrixMatrixSimple"

