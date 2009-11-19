{-# LANGUAGE Rank2Types #-}
module EqManips.Algorithm.Eval( reduce
                              , exactReduce 
                              , evalGlobalLossyStatement 
                              , evalGlobalLosslessStatement 
                              ) where

import Data.Maybe

import qualified EqManips.ErrorMessages as Err
import Control.Applicative
import EqManips.Types
import EqManips.Polynome
import EqManips.Propreties
import EqManips.EvaluationContext
import EqManips.Algorithm.Cleanup
import EqManips.Algorithm.Inject
import EqManips.Algorithm.Derivative
import EqManips.Algorithm.Utils
import EqManips.Algorithm.MetaEval
import EqManips.Algorithm.EvalFloating
import EqManips.Algorithm.EvalPolynomial

import EqManips.Algorithm.Unification
import EqManips.Algorithm.EvalTypes

import Data.List( foldl' , transpose, sort )


--------------------------------------------------
----            Eval helpers
--------------------------------------------------
-- | Used to transform a binop to a scalar if size
-- is small
binOp :: BinOperator -> [FormulaPrim] -> FormulaPrim
binOp _ [x] = x
binOp op lst = BinOp op lst

-- | Evaluate a binary operator
binEval :: BinOperator -> EvalOp -> EvalOp -> [FormulaPrim] -> EqContext FormulaPrim
binEval op f inv formulaList 
    | op `hasProp` Associativ && op `hasProp` Commutativ = do
#ifdef _DEBUG
        addTrace ("Sorting => ", treeIfyFormula . Formula $ BinOp op formulaList)
#endif
        biAssocM f inv (sort formulaList) >>= return . binOp op

    | otherwise = do
#ifdef _DEBUG
        addTrace ("Basic Eval=>", treeIfyFormula . Formula $ BinOp op formulaList)
#endif
        biAssocM f inv formulaList >>= return . binOp op

evalGlobalLossyStatement, evalGlobalLosslessStatement :: FormulaEvaluator
evalGlobalLossyStatement = evalGlobalStatement reduce'
evalGlobalLosslessStatement = evalGlobalStatement exactReduce'

-- | Main function to evaluate another function
reduce :: FormulaEvaluator
reduce = taggedEvaluator reduce'

-- | Main function to evaluate raw formula
reduce' :: EvalFun
reduce' f = (eval reduce' $ cleaner f)
        >>= polyEvalRules 
        >>= floatEvalRules
    where cleaner = unTagFormula . cleanupRules . Formula

-- | Only perform non-lossy transformations
exactReduce :: FormulaEvaluator
exactReduce = taggedEvaluator exactReduce'

-- | same as exactReduce, but perform on raw formula.
exactReduce' :: EvalFun
exactReduce' f = (eval exactReduce' $ cleaner f)
           >>= polyEvalRules 
    where cleaner = unTagFormula . cleanupRules . Formula

left :: (Monad m) => a -> m (Either a b)
left = return . Left

right :: (Monad m) => b -> m (Either a b)
right = return . Right

-----------------------------------------------
----        Top level evaluation
-----------------------------------------------
-- | Add a function into the symbol table.
addLambda :: String -> [Formula ListForm] -> Formula ListForm -> EqContext ()
addLambda varName args body = do
    symb <- symbolLookup varName
    case symb of
      Nothing -> addSymbol varName . Formula
                    $ Lambda [(map unTagFormula args, unTagFormula body)]
      Just (Formula (Lambda clauses@((prevArg,_):_))) -> do
          if length prevArg /= length args
            then do
             eqFail (Formula $ Variable varName) Err.def_diff_argcount
             return ()
            else updateSymbol varName . Formula . Lambda 
                            $ clauses ++ [(map unTagFormula args
                                          , unTagFormula body)]
          
      Just _ -> do
         eqFail (Formula $ Variable varName) $ Err.def_not_lambda varName
         return ()

-- | Add a "value" into the symbol table
addVar :: String -> Formula ListForm -> EqContext ()
addVar varName body = do
    symb <- symbolLookup varName
    case symb of
      Nothing -> addSymbol varName body
      Just _ -> do
         eqFail (Formula $ Variable varName) $ Err.def_already varName
         return ()

-- | Evaluate top level declarations
evalGlobalStatement :: EvalFun -> Formula ListForm -> EqContext (Formula ListForm)
evalGlobalStatement _ (Formula (BinOp OpAttrib [ (App (Variable funName) argList)
                                               , body ])) = do
    addLambda funName (map Formula argList) (Formula body)
    return $ Formula (BinOp OpEq [(App (Variable funName) argList), body])

evalGlobalStatement evaluator (Formula (BinOp OpAttrib [(Variable varName), body])) = do
    pushContext
    body' <- evaluator body
    popContext
    addVar varName (Formula body')
    return $ Formula (BinOp OpEq [(Variable varName), body'])

evalGlobalStatement evaluator (Formula e) = do
    pushContext
    a <- evaluator e
    popContext
    return $ Formula a

-----------------------------------------------
----            '+'
-----------------------------------------------
add :: EvalFun -> EvalOp
add _ (CInteger i1) (CInteger i2) = left . CInteger $ i1 + i2
add _ (Poly p1) (Poly p2) = left . Poly $ p1 + p2
add _ v1 (Poly p) | isFormulaScalar v1 = left . Poly $ polyCoeffMap (\a -> (scalarToCoeff v1) + a) p
add _ (Poly p) v2 | isFormulaScalar v2 = left . Poly $ polyCoeffMap (+ (scalarToCoeff v2)) p

add evaluator f1@(Matrix _ _ _) f2@(Matrix _ _ _) =
    matrixMatrixSimple evaluator (+) f1 f2
add _ f1@(Matrix _ _ _) f2 = do
    eqPrimFail (f1+f2) Err.add_matrix
    right (f1, f2)
add _ f1 f2@(Matrix _ _ _) = do
    eqPrimFail (f1+f2) Err.add_matrix
    right (f1, f2)
add _ e e' = right (e, e')

-----------------------------------------------
----            '-'
-----------------------------------------------
sub :: EvalFun -> EvalOp
sub _ (CInteger i1) (CInteger i2) = left . CInteger $ i1 - i2
sub _ (Poly p1) (Poly p2) = left . Poly $ p1 - p2
sub _ v1 (Poly p) | isFormulaScalar v1 = left . Poly $ polyCoeffMap (\a -> (scalarToCoeff v1) - a) p
sub _ (Poly p) v2 | isFormulaScalar v2 = left . Poly $ polyCoeffMap (\a -> a - (scalarToCoeff v2)) p
sub evaluator f1@(Matrix _ _ _) f2@(Matrix _ _ _) =
    matrixMatrixSimple evaluator (-) f1 f2
sub _ f1@(Matrix _ _ _) f2 = do
    eqPrimFail (f1-f2) Err.sub_matrix
    right (f1, f2)
sub _ f1 f2@(Matrix _ _ _) = do
    eqPrimFail (f1-f2) Err.sub_matrix
    right (f1, f2)
sub _ e e' = right (e,e')

-----------------------------------------------
----            '*'
-----------------------------------------------
mul :: EvalFun -> EvalOp
mul _ (CInteger i1) (CInteger i2) = left . CInteger $ i1 * i2
mul _ (Poly p1) (Poly p2) = left . Poly $ p1 * p2
mul _ v1 (Poly p) | isFormulaScalar v1 = left . Poly $ polyCoeffMap (\a -> (scalarToCoeff v1) * a) p
mul _ (Poly p) v2 | isFormulaScalar v2 = left . Poly $ polyCoeffMap (* (scalarToCoeff v2)) p
mul evaluator f1@(Matrix _ _ _) f2@(Matrix _ _ _) = matrixMatrixMul evaluator f1 f2
mul evaluator m@(Matrix _ _ _) s = matrixScalar evaluator (*) m s >>= left
mul evaluator s m@(Matrix _ _ _) = matrixScalar evaluator (*) m s >>= left
mul _ e e' = right (e, e')

-----------------------------------------------
----        '/'
-----------------------------------------------
-- | Handle the division operator. Nicely handle the case
-- of division by 0.
division :: EvalFun -> EvalOp
division _ l@(Matrix _ _ _) r@(Matrix _ _ _) = do
    eqPrimFail (l / r) Err.div_undefined_matrixes
    left $ Block 1 1 1

division _ f1 f2@(CInteger 0) = do
    eqPrimFail (f1 / f2) Err.div_by_0
    left $ Block 1 1 1

division _ f1 f2@(CFloat 0) = do
    eqPrimFail (f1 / f2) Err.div_by_0
    left $ Block 1 1 1

division _ (CInteger i1) (CInteger i2)
    | i1 `mod` i2 == 0 = left . CInteger $ i1 `div` i2

division _ v1 (Poly p) | isFormulaScalar v1 = left . Poly $ polyCoeffMap (\a -> (scalarToCoeff v1) / a) p
division _ (Poly p) v2 | isFormulaScalar v2 = left . Poly $ polyCoeffMap (/ (scalarToCoeff v2)) p
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
factorial :: EvalFun
factorial f@(CFloat _) = eqPrimFail f Err.factorial_on_real 
factorial (CInteger 0) = return $ CInteger 1
factorial f@(CInteger i) | i > 0 = return . CInteger $ product [1 .. i]
                         | otherwise = eqPrimFail f Err.factorial_negative
factorial f@(Matrix _ _ _) = eqPrimFail f Err.factorial_matrix
factorial a = return $ UnOp OpFactorial a

-----------------------------------------------
----        'floor'
-----------------------------------------------
floorEval :: EvalFun
floorEval i@(CInteger _) = return i
floorEval f = return $ UnOp OpFloor f

-----------------------------------------------
----        'frac'
-----------------------------------------------
fracEval :: EvalFun
fracEval (CInteger _) = return $ CInteger 0
fracEval f = return $ UnOp OpFrac f

-----------------------------------------------
----        'Ceil'
-----------------------------------------------
ceilEval :: EvalFun
ceilEval i@(CInteger _) = return i
ceilEval f = return $ UnOp OpCeil f

-----------------------------------------------
----        'negate'
-----------------------------------------------
fNegate :: EvalFun
fNegate (CInteger i) = return . CInteger $ negate i
fNegate (UnOp OpNegate f) = return f
fNegate f = return $ negate f

-----------------------------------------------
----        'abs'
-----------------------------------------------
fAbs :: EvalFun
fAbs (CInteger i) = return . CInteger $ abs i
fAbs f = return $ abs f

-----------------------------------------------
----        'Comparison operators'
-----------------------------------------------
predicateList :: BinOperator -> EvalPredicate -> [FormulaPrim] -> EqContext FormulaPrim
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
             -> FormulaPrim -> FormulaPrim
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
metaEvaluation :: EvalFun -> MetaOperation -> EvalFun
metaEvaluation evaluator m f = unTagFormula
              <$> metaEval (taggedEvaluator evaluator) m (Formula f)

-----------------------------------------------
----        General evaluation
-----------------------------------------------
-- | General evaluation/reduction function
eval :: EvalFun -> EvalFun
eval evaluator (Meta m f) = metaEvaluation evaluator m f
eval _ (NumEntity Pi) = return $ CFloat pi
eval evaluator (Matrix n m mlines) = do
    cells <- sequence [mapM evaluator line | line <- mlines]
    return $ Matrix n m cells

eval _ func@(Lambda _) = unTagFormula <$> inject (Formula func)
eval _ (Variable v) = do
    symbol <- symbolLookup v
    case symbol of
         Nothing -> return $ Variable v
         Just (Formula (f)) -> return f

eval evaluator (App def var) = do
    redDef <- evaluator def
    redVar <- mapM evaluator var
#ifdef _DEBUG
    addTrace ("Appbegin |", treeIfyFormula . Formula $ App redDef redVar)
#endif
    needApply redDef redVar
   where needApply :: FormulaPrim -> [FormulaPrim] -> EqContext FormulaPrim
         needApply (Lambda funArgs) args' =
           case getFirstUnifying funArgs args' of
                Nothing -> eqPrimFail (App def var) Err.app_no_applygindef
                Just (body, subst) -> do
                    pushContext
                    addSymbols [ (name, Formula formula) 
                                        | (name, formula) <- subst]
#ifdef _DEBUG
                    {-traceContext-}
                    addTrace ("subst | " ++ show subst, treeIfyFormula $ Formula body)
#endif
                    body' <- evaluator body
#ifdef _DEBUG
                    addTrace ("body' | " ++ show body', treeIfyFormula $ Formula body')
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

eval _ f@(BinOp OpAttrib _) = eqPrimFail f Err.attrib_in_expr 

eval evaluator (UnOp OpFactorial f) = factorial =<< evaluator f
eval evaluator (UnOp OpFloor f) = floorEval =<< evaluator f
eval evaluator (UnOp OpCeil f) = ceilEval =<< evaluator f
eval evaluator (UnOp OpFrac f) = fracEval =<< evaluator f

eval evaluator (UnOp OpNegate f) = fNegate =<< evaluator f
eval evaluator (UnOp OpAbs f) = fAbs =<< evaluator f

eval evaluator (UnOp op f) = return . UnOp op =<< evaluator f

eval evaluator (Derivate what (Meta op var)) =
    metaEvaluation evaluator op var >>= (\a -> eval evaluator (Derivate what a))

eval evaluator (Derivate what (Variable s)) = do
#ifdef _DEBUG
    addTrace ("Derivation on " ++ s, treeIfyFormula . Formula $ what)
#endif
    derived <- derivate (taggedEvaluator evaluator) s (treeIfyFormula $ Formula what)
    return . unTagFormula $ cleanup derived

eval _ f@(Derivate _ _) =
    eqPrimFail f Err.deriv_bad_var_spec 

eval evaluator formu@(Sum (BinOp OpEq [Variable v, inexpr]) endexpr f) = do
    inexpr' <- evaluator inexpr
    endexpr' <- evaluator endexpr
    sumEval inexpr' endexpr'
     where sumEval (CInteger initi) (CInteger endi)
            | initi <= endi = iterateFormula evaluator (BinOp OpAdd) v initi endi f
            | otherwise = eqPrimFail formu Err.sum_wrong_bounds
           sumEval ini end = return $ Sum (BinOp OpEq [Variable v, ini]) end f
    

eval evaluator formu@(Product (BinOp OpEq [Variable v, inexpr]) endexpr f) = do
    inexpr' <- evaluator inexpr
    endexpr' <- evaluator endexpr
    prodEval inexpr' endexpr'
     where prodEval (CInteger initi) (CInteger endi)
            | initi <= endi = iterateFormula evaluator (BinOp OpMul) v initi endi f
            | otherwise = eqPrimFail formu Err.sum_wrong_bounds
           prodEval ini end = return $ Product (BinOp OpEq [Variable v, ini]) end f
    
eval _ f@(Integrate _ _ _ _) =
    eqPrimFail f Err.integration_no_eval

eval _ f@(Block _ _ _) = eqPrimFail f Err.block_eval
eval _ end = return end

--------------------------------------------------------------
---- iteration
--------------------------------------------------------------
iterateFormula :: EvalFun
               -> ([FormulaPrim] -> FormulaPrim)
               -> String -> Integer -> Integer -> FormulaPrim
               -> EqContext FormulaPrim
iterateFormula evaluator op ivar initi endi what = do
    pushContext
    rez <- mapM combiner [initi .. endi]
    popContext
    case rez of
         [x] -> evaluator x
         _  -> evaluator $ op rez
     where combiner i = do
               addSymbol ivar (Formula $ CInteger i)
               unTagFormula <$> inject (Formula what)

--------------------------------------------------------------
---- Matrix related functions
--------------------------------------------------------------
matrixScalar :: EvalFun
             -> FormulOperator
             -> FormulaPrim -> FormulaPrim
             -> EqContext FormulaPrim
matrixScalar evaluator op s m@(Matrix _ _ _) = matrixScalar evaluator op m s
matrixScalar evaluator op (Matrix n m mlines) s = cell >>= return . Matrix n m
    where cell = sequence
            [ mapM (\c -> evaluator $ c `op` s) line | line <- mlines]
matrixScalar _ _ _ _ = error Err.matrixScalar_badop

-- | Multiplication between two matrix. Check for matrix sizes.
matrixMatrixMul :: EvalFun -> EvalOp
matrixMatrixMul evaluator m1@(Matrix n _ mlines) m2@(Matrix n' m' mlines')
    | n /= m' = do eqFail (Formula $ BinOp OpMul [m1, m2]) Err.matrix_mul_bad_size
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
matrixMatrixSimple :: EvalFun
                   -> FormulOperator
                   -> FormulaPrim -> FormulaPrim
                   -> EqContext (Either FormulaPrim (FormulaPrim,FormulaPrim))
matrixMatrixSimple evaluator op m1@(Matrix n m mlines) m2@(Matrix n' m' mlines')
    | n /= n' || m /= m' = do
        eqFail (Formula $ m1 `op` m2) Err.matrix_diff_size
        return $ Right (m1, m2)
    | otherwise = newCells >>= return . Left . Matrix n m
        where dop (e1, e2) = evaluator $ e1 `op`e2
              newCells = sequence [ mapM dop $ zip line1 line2
                                     | (line1, line2) <- zip mlines mlines']
matrixMatrixSimple _ _ _ _ = error $ Err.shouldnt_happen "matrixMatrixSimple"

