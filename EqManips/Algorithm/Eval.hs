{-# LANGUAGE Rank2Types #-}
module EqManips.Algorithm.Eval( reduce
                              , exactReduce 
                              , evalGlobalLossyStatement 
                              , evalGlobalLosslessStatement 
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
import EqManips.Algorithm.EvalFloating

import EqManips.Algorithm.Unification

import Data.List( foldl' , transpose, sort )

type FormulOperator = Formula -> Formula -> Formula
type EvalOp = Formula -> Formula -> EqContext (Either Formula (Formula,Formula))
type Evaluator = Formula -> EqContext Formula

evalGlobalLossyStatement, evalGlobalLosslessStatement :: Evaluator
evalGlobalLossyStatement = evalGlobalStatement reduce
evalGlobalLosslessStatement = evalGlobalStatement exactReduce

-- | Main function to evaluate another function
reduce :: Formula -> EqContext Formula
reduce f = (eval reduce $ cleanupRules f) >>= floatEvalRules

exactReduce :: Formula -> EqContext Formula
exactReduce = eval exactReduce . cleanupRules

left :: (Monad m) => a -> m (Either a b)
left = return . Left

right :: (Monad m) => b -> m (Either a b)
right = return . Right

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
evalGlobalStatement :: Evaluator -> Formula -> EqContext Formula
evalGlobalStatement _ (BinOp OpAttrib [ (App (Variable funName) argList)
                                , body ]) = do
    addLambda funName argList body
    return $ (BinOp OpEq [(App (Variable funName) argList), body])

evalGlobalStatement evaluator (BinOp OpAttrib [(Variable varName), body]) = do
    pushContext
    body' <- evaluator body
    popContext
    addVar varName body'
    return $ (BinOp OpEq [(Variable varName), body'])

evalGlobalStatement evaluator e = do
    pushContext
    a <- evaluator e
    popContext
    return a

-----------------------------------------------
----            '+'
-----------------------------------------------
add :: Evaluator -> EvalOp
add _ (CInteger i1) (CInteger i2) = left . CInteger $ i1 + i2
add evaluator f1@(Matrix _ _ _) f2@(Matrix _ _ _) =
    matrixMatrixSimple evaluator (+) f1 f2
add _ f1@(Matrix _ _ _) f2 = do
    eqFail (f1+f2) Err.add_matrix
    right (f1, f2)
add _ f1 f2@(Matrix _ _ _) = do
    eqFail (f1+f2) Err.add_matrix
    right (f1, f2)

add _ e e' = right (e, e')

-----------------------------------------------
----            '-'
-----------------------------------------------
sub :: Evaluator -> EvalOp
sub _ (CInteger i1) (CInteger i2) = left . CInteger $ i1 - i2
sub evaluator f1@(Matrix _ _ _) f2@(Matrix _ _ _) =
    matrixMatrixSimple evaluator (-) f1 f2
sub _ f1@(Matrix _ _ _) f2 = do
    eqFail (f1-f2) Err.sub_matrix
    right (f1, f2)
sub _ f1 f2@(Matrix _ _ _) = do
    eqFail (f1-f2) Err.sub_matrix
    right (f1, f2)
sub _ e e' = right (e,e')

-----------------------------------------------
----            '*'
-----------------------------------------------
mul :: Evaluator -> EvalOp
mul _ (CInteger i1) (CInteger i2) = left . CInteger $ i1 * i2
mul evaluator f1@(Matrix _ _ _) f2@(Matrix _ _ _) = matrixMatrixMul evaluator f1 f2
mul evaluator m@(Matrix _ _ _) s = matrixScalar evaluator (*) m s >>= left
mul evaluator s m@(Matrix _ _ _) = matrixScalar evaluator (*) m s >>= left
mul _ e e' = right (e, e')

-----------------------------------------------
----        '/'
-----------------------------------------------
-- | Handle the division operator. Nicely handle the case
-- of division by 0.
division :: Evaluator -> EvalOp
division _ l@(Matrix _ _ _) r@(Matrix _ _ _) = do
    eqFail (l / r) Err.div_undefined_matrixes
    left $ Block 1 1 1

division _ f1 f2@(CInteger 0) = do
    eqFail (f1 / f2) Err.div_by_0
    left $ Block 1 1 1

division _ f1 f2@(CFloat 0) = do
    eqFail (f1 / f2) Err.div_by_0
    left $ Block 1 1 1

division _ (CInteger i1) (CInteger i2)
    | i1 `mod` i2 == 0 = left . CInteger $ i1 `div` i2
division evaluator m@(Matrix _ _ _) s = matrixScalar evaluator (/) m s >>= left
division evaluator s m@(Matrix _ _ _) = matrixScalar evaluator (/) m s >>= left
division _ f1 f2 = right (f1, f2)

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
floorEval f = return $ UnOp OpFloor f

-----------------------------------------------
----        'frac'
-----------------------------------------------
fracEval :: Formula -> EqContext Formula
fracEval (CInteger _) = return $ CInteger 0
fracEval f = return $ UnOp OpFrac f

-----------------------------------------------
----        'Ceil'
-----------------------------------------------
ceilEval :: Formula -> EqContext Formula
ceilEval i@(CInteger _) = return i
ceilEval f = return $ UnOp OpCeil f

-----------------------------------------------
----        'negate'
-----------------------------------------------
fNegate :: Formula -> EqContext Formula
fNegate (CInteger i) = return . CInteger $ negate i
fNegate (UnOp OpNegate f) = return f
fNegate f = return $ negate f

-----------------------------------------------
----        'abs'
-----------------------------------------------
fAbs :: Formula -> EqContext Formula
fAbs (CInteger i) = return . CInteger $ abs i
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
        biAssocM f inv formulaList >>= return . binOp op

-----------------------------------------------
----        General evaluation
-----------------------------------------------
-- | General evaluation/reduction function
eval :: Evaluator -> Evaluator
eval evaluator (Meta m f) = metaEval evaluator m f
eval _ (NumEntity Pi) = return $ CFloat pi
eval evaluator (Matrix n m mlines) = do
    cells <- sequence [mapM evaluator line | line <- mlines]
    return $ Matrix n m cells

eval _ func@(Lambda _) = inject func
eval _ (Variable v) = symbolLookup v
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

eval evaluator (BinOp OpAdd fs) = do
    binEval OpAdd (add evaluator) (add evaluator) =<< mapM evaluator fs
eval evaluator (BinOp OpSub fs) =
    binEval OpSub (sub evaluator) (add evaluator) =<< mapM evaluator fs
eval evaluator (BinOp OpMul fs) =
    binEval OpMul (mul evaluator) (mul evaluator) =<< mapM evaluator fs

-- | Todo fix this, it's incorrect
eval evaluator (BinOp OpPow fs) = binEval OpPow power power =<< mapM evaluator fs
eval evaluator (BinOp OpDiv fs) =
    binEval OpDiv (division evaluator) (mul evaluator) =<< mapM evaluator fs

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

-- | Special case for programs, don't evaluate left :]
eval evaluator (BinOp OpAttrib [a,b]) =
    evaluator b >>= return . BinOp OpAttrib . (a:) . (:[])

eval _ f@(BinOp OpAttrib _) = eqFail f Err.attrib_in_expr 

eval evaluator (UnOp OpFactorial f) = factorial =<< evaluator f
eval evaluator (UnOp OpFloor f) = floorEval =<< evaluator f
eval evaluator (UnOp OpCeil f) = ceilEval =<< evaluator f
eval evaluator (UnOp OpFrac f) = fracEval =<< evaluator f

eval evaluator (UnOp OpNegate f) = fNegate =<< evaluator f
eval evaluator (UnOp OpAbs f) = fAbs =<< evaluator f

eval evaluator (UnOp op f) = return . UnOp op =<< evaluator f

eval evaluator (Derivate what (Meta op var)) = do
    evalued <- metaEval evaluator op var
    evaluator $ Derivate what evalued

eval evaluator (Derivate f@(Meta op _) var) = do
    evalued <- metaEval evaluator op f
    evaluator (Derivate evalued var)

eval evaluator (Derivate what (Variable s)) = do
#ifdef _DEBUG
    addTrace ("Derivation on " ++ s, what)
#endif
    derived <- derivate evaluator s what
    return $ cleanup derived

eval _ f@(Derivate _ _) =
    eqFail f Err.deriv_bad_var_spec 

eval evaluator formu@(Sum (BinOp OpEq [Variable v, inexpr]) endexpr f) = do
    inexpr' <- evaluator inexpr
    endexpr' <- evaluator endexpr
    sumEval inexpr' endexpr'
     where sumEval (CInteger initi) (CInteger endi)
            | initi <= endi = iterateFormula evaluator (BinOp OpAdd) v initi endi f
            | otherwise = eqFail formu Err.sum_wrong_bounds
           sumEval ini end = return $ Sum (BinOp OpEq [Variable v, ini]) end f
    

eval evaluator formu@(Product (BinOp OpEq [Variable v, inexpr]) endexpr f) = do
    inexpr' <- evaluator inexpr
    endexpr' <- evaluator endexpr
    prodEval inexpr' endexpr'
     where prodEval (CInteger initi) (CInteger endi)
            | initi <= endi = iterateFormula evaluator (BinOp OpMul) v initi endi f
            | otherwise = eqFail formu Err.sum_wrong_bounds
           prodEval ini end = return $ Product (BinOp OpEq [Variable v, ini]) end f
    
eval _ f@(Integrate _ _ _ _) =
    eqFail f Err.integration_no_eval

eval _ f@(Block _ _ _) = eqFail f Err.block_eval
eval _ end = return end

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
matrixScalar :: Evaluator
             -> (Formula -> Formula -> Formula) -> Formula -> Formula 
             -> EqContext Formula
matrixScalar evaluator op s m@(Matrix _ _ _) = matrixScalar evaluator op m s
matrixScalar evaluator op (Matrix n m mlines) s = cell >>= return . Matrix n m
    where cell = sequence
            [ mapM (\c -> evaluator $ c `op` s) line | line <- mlines]
matrixScalar _ _ _ _ = error Err.matrixScalar_badop

-- | Multiplication between two matrix. Check for matrix sizes.
matrixMatrixMul :: Evaluator -> EvalOp
matrixMatrixMul evaluator m1@(Matrix n _ mlines) m2@(Matrix n' m' mlines')
    | n /= m' = do eqFail (BinOp OpMul [m1, m2]) Err.matrix_mul_bad_size
                   right (m1, m2)
    | otherwise = cellLine >>= left . Matrix n n'
        where cellLine = sequence
                    [ sequence [multCell $ zip line row | row <- transpose mlines' ]
                                                        | line <- mlines]

              multCell l = evaluator $ foldl' multAtor (initCase l) (tail l)
              multAtor acc (l, r) = acc + (l * r)

              initCase ((x,y):_) = x * y
              initCase _ = error . Err.shouldnt_happen $ Err.matrix_empty ++ " - "
              
matrixMatrixMul _ _ _ = error $ Err.shouldnt_happen "matrixMatrixMul - "

-- | Simple operation, matrix addition or substraction
matrixMatrixSimple :: Evaluator -> FormulOperator -> Formula -> Formula 
                   -> EqContext (Either Formula (Formula,Formula))
matrixMatrixSimple evaluator op m1@(Matrix n m mlines) m2@(Matrix n' m' mlines')
    | n /= n' || m /= m' = do
        eqFail (m1 `op` m2) Err.matrix_diff_size
        return $ Right (m1, m2)
    | otherwise = newCells >>= return . Left . Matrix n m
        where dop (e1, e2) = evaluator $ e1 `op`e2
              newCells = sequence [ mapM dop $ zip line1 line2
                                     | (line1, line2) <- zip mlines mlines']
matrixMatrixSimple _ _ _ _ = error $ Err.shouldnt_happen "matrixMatrixSimple"

