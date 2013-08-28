{-# LANGUAGE Rank2Types #-}
module Language.Eq.Algorithm.Eval.GenericEval ( eval ) where

import Data.Ratio

import qualified Language.Eq.ErrorMessages as Err
import Control.Applicative
import Language.Eq.Types
import Language.Eq.Conf
import Language.Eq.EvaluationContext
import Language.Eq.Algorithm.Cleanup
import Language.Eq.Algorithm.Inject
import Language.Eq.Algorithm.Derivative
import Language.Eq.Algorithm.Utils
import Language.Eq.Algorithm.Eval.Meta

import Language.Eq.Algorithm.Unification
import Language.Eq.Algorithm.Eval.Types
import Language.Eq.Algorithm.Eval.Utils

import Data.List( transpose, foldl' )

-----------------------------------------------
----            '+'
-----------------------------------------------
add :: EvalFun -> EvalOp
add _ (CInteger i1) (CInteger i2) = left . CInteger $ i1 + i2
-- Handle negation, as we may not know which cleaning has been performed
-- on the formula.
add _ (CInteger i1) (UnOp _ OpNegate (CInteger i2)) = left . CInteger $ i1 - i2
add _ (UnOp _ OpNegate (CInteger i1)) (CInteger i2) = left . CInteger $ negate i1 + i2
add _ (UnOp _ OpNegate (CInteger i1)) (UnOp _ OpNegate (CInteger i2)) =
        left . CInteger $ negate i1 + negate i2
add evaluator f1@(Matrix _ _ _ _) f2@(Matrix _ _ _ _) =
    matrixMatrixSimple evaluator (+) f1 f2
add _ f1@(Matrix _ _ _ _) f2 = do
    _ <- eqPrimFail (f1+f2) Err.add_matrix
    right (f1, f2)
add _ f1 f2@(Matrix _ _ _ _) = do
    _ <- eqPrimFail (f1+f2) Err.add_matrix
    right (f1, f2)
add _ e e' = right (e, e')

-----------------------------------------------
----            '-'
-----------------------------------------------
sub :: EvalFun -> EvalOp
sub _ (CInteger i1) (CInteger i2) = left . CInteger $ i1 - i2
sub _ (CInteger i1) (UnOp _ OpNegate (CInteger i2)) = left . CInteger $ i1 - negate i2
sub _ (UnOp _ OpNegate (CInteger i1)) (CInteger i2) = left . CInteger $ negate i1 - i2
sub _ (UnOp _ OpNegate (CInteger i1)) (UnOp _ OpNegate (CInteger i2)) =
        left . CInteger $ negate i1 - negate i2
sub evaluator f1@(Matrix _ _ _ _) f2@(Matrix _ _ _ _) =
    matrixMatrixSimple evaluator (-) f1 f2
sub _ f1@(Matrix _ _ _ _) f2 = do
    _ <- eqPrimFail (f1-f2) Err.sub_matrix
    right (f1, f2)
sub _ f1 f2@(Matrix _ _ _ _) = do
    _ <- eqPrimFail (f1-f2) Err.sub_matrix
    right (f1, f2)
sub _ e e' = right (e,e')

-----------------------------------------------
----            '*'
-----------------------------------------------
mul :: EvalFun -> EvalOp
mul _ (CInteger i1) (CInteger i2) = left . CInteger $ i1 * i2
mul _ (CInteger i1) (UnOp _ OpNegate (CInteger i2)) = left . CInteger $ i1 * negate i2
mul _ (UnOp _ OpNegate (CInteger i1)) (CInteger i2) = left . CInteger $ negate i1 * i2
mul _ (UnOp _ OpNegate (CInteger i1)) (UnOp _ OpNegate (CInteger i2)) =
        left . CInteger $ i1 * i2
mul evaluator f1@(Matrix _ _ _ _) f2@(Matrix _ _ _ _) = matrixMatrixMul evaluator f1 f2
mul evaluator m@(Matrix _ _ _ _) s = matrixScalar evaluator (*) m s >>= left
mul evaluator s m@(Matrix _ _ _ _) = matrixScalar evaluator (*) m s >>= left
mul _ e e' = right (e, e')

-----------------------------------------------
----        '/'
-----------------------------------------------
-- | Handle the division operator. Nicely handle the case
-- of division by 0.
division :: EvalFun -> EvalOp
division _ l@(Matrix _ _ _ _) r@(Matrix _ _ _ _) = do
    _ <- eqPrimFail (l / r) Err.div_undefined_matrixes
    left $ Block 1 1 1

division _ f1 f2@(CInteger 0) = do
    _ <- eqPrimFail (f1 / f2) Err.div_by_0
    left $ Block 1 1 1

division _ f1 f2@(CFloat 0) = do
    _ <- eqPrimFail (f1 / f2) Err.div_by_0
    left $ Block 1 1 1

division _ (CInteger i1) (CInteger i2)
    | i1 `mod` i2 == 0 = left . CInteger $ i1 `div` i2

division _ (CInteger i1) (UnOp _ OpNegate (CInteger i2))
    | i1 `mod` i2 == 0 = left . negate . CInteger $ i1 `div` i2

division _ (UnOp _ OpNegate (CInteger i1)) (CInteger i2)
    | i1 `mod` i2 == 0 = left . negate . CInteger $ i1 `div` i2

division _ (UnOp _ OpNegate (CInteger i1)) (UnOp _ OpNegate (CInteger i2))
    | i1 `mod` i2 == 0 = left . CInteger $ i1 `div` i2

division evaluator m@(Matrix _ _ _ _) s = matrixScalar evaluator (/) m s >>= left
division evaluator s m@(Matrix _ _ _ _) = matrixScalar evaluator (/) m s >>= left
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
factorial f@(Matrix _ _ _ _) = eqPrimFail f Err.factorial_matrix
factorial a = return $ unOp OpFactorial a

-----------------------------------------------
----        'floor'
-----------------------------------------------
floorEval :: EvalFun
floorEval i@(CInteger _) = return i
floorEval f = return $ unOp OpFloor f

-----------------------------------------------
----        'frac'
-----------------------------------------------
fracEval :: EvalFun
fracEval (CInteger _) = return $ CInteger 0
fracEval f = return $ unOp OpFrac f

--------------------------------------------------
----            'matrixWidth'
--------------------------------------------------
matrixWidthEval :: EvalFun
matrixWidthEval (Matrix _ _ width _) = return . CInteger $ toInteger width
matrixWidthEval f = return $ unOp OpMatrixWidth f

--------------------------------------------------
----            'matrixHeight'
--------------------------------------------------
matrixHeightEval :: EvalFun
matrixHeightEval (Matrix _ height _ _) = return . CInteger $ toInteger height
matrixHeightEval f = return $ unOp OpMatrixHeight f

-----------------------------------------------
----        'Ceil'
-----------------------------------------------
ceilEval :: EvalFun
ceilEval i@(CInteger _) = return i
ceilEval f = return $ unOp OpCeil f

-----------------------------------------------
----        'negate'
-----------------------------------------------
fNegate :: EvalFun
fNegate (CInteger i) = return . CInteger $ negate i
fNegate (UnOp _ OpNegate f) = return f
fNegate f = return $ negate f

-----------------------------------------------
----        'abs'
-----------------------------------------------
fAbs :: EvalFun
fAbs (CInteger i) = return . CInteger $ abs i
fAbs (UnOp _ OpNegate (CInteger i)) = return . CInteger $ abs i
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
          lastRez (lst,_,_) = return $ binOp op lst


equality, inequality :: [FormulaPrim] -> EqContext FormulaPrim
equality = eqApplying (==) OpEq
inequality = eqApplying (/=) OpNe

eqApplying :: (forall a. Eq a => a -> a -> Bool) -> BinOperator
           -> [FormulaPrim] -> EqContext FormulaPrim
eqApplying _ _ [] = return $ Block 1 1 1
eqApplying f op (x:xs) = return . reOp . fst $ foldr applyer (Just [x], x) xs
    where reOp Nothing = Truth False
          reOp (Just [_]) = Truth True
          reOp (Just a) = binOp op a

          applyer val (Nothing, _) = (Nothing, val)
          applyer val (Just acc, prev) = case equalityOperator f prev val of
                Nothing -> (Just $ val : acc, val)
                Just False -> (Nothing, val)
                Just True -> (Just acc, val)

-- | In charge of implementing the casting for '=' and '/='
-- operators.
equalityOperator :: (forall a. Eq a => a -> a -> Bool)
                 -> FormulaPrim -> FormulaPrim
                 -> Maybe Bool
equalityOperator f (CInteger a) (CInteger b) = Just $ f a b

-- Fraction/Int
equalityOperator f (Fraction a) (Fraction b) = Just $ f a b
equalityOperator f (CInteger a) (Fraction b) = Just $ f (a % 1) b
equalityOperator f (Fraction a) (CInteger b) = Just $ f a (b % 1)

-- Float/Int
equalityOperator f (CFloat a) (CFloat b) = Just $ f a b
equalityOperator f a@(CFloat _) (CInteger b) =
    equalityOperator f a . CFloat $ fromIntegral b
equalityOperator f (CInteger a) b@(CFloat _) =
    equalityOperator f (CFloat $ fromIntegral a) b

-- Complex/Other
equalityOperator f (Complex _ (r1, i1)) (Complex _ (r2, i2)) =
    (&&) <$> equalityOperator f r1 r2
         <*> equalityOperator f i1 i2

equalityOperator f number a@(Complex _ (r, i)) 
    | isFormulaScalar a = (&&) <$> equalityOperator f number r
                               <*> equalityOperator f (CInteger 0) i
equalityOperator _ _ _ = Nothing


-- | Casting for comparaison operator.
compOperator :: (forall a. Ord a => a -> a -> Bool)
             -> FormulaPrim -> FormulaPrim
             -> Maybe Bool
compOperator f (CInteger a) (CInteger b) = Just $ f a b
compOperator f (CFloat a) (CFloat b) = Just $ f a b
compOperator f (Fraction a) (Fraction b) = Just $ f a b
compOperator f (CInteger a) (Fraction b) = Just $ f (a % 1) b
compOperator f (Fraction a) (CInteger b) = Just $ f a (b % 1)
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

-- | Used to create matrix from lists
matrixCreate :: [FormulaPrim] -> EqContext FormulaPrim
matrixCreate [List _ whole@(List _ subList:rest)]
  | and $ map isAllList rest =
      pure . matrix rowCount columnsCount $ map subListExtract whole
    where columnsCount = length subList
          rowCount = length rest + 1

          isAllList (List _ lst) = length lst == columnsCount
          isAllList _ = False

          subListExtract (List _ lst) = lst
          subListExtract _ = error "Extracting sublist of non-list"

matrixCreate [(List _ elems)] = pure $ matrix 1 (length elems) [elems]

matrixCreate [CInteger 1, CInteger m, List _ elems]
    | length elems == (fromInteger m) =
        return $ matrix 1 (fromInteger m) [elems]

matrixCreate [CInteger n, CInteger 1, List _ elems]
    | length elems == (fromInteger n) =
        return . matrix (fromInteger n) 1 $ map (:[]) elems

matrixCreate args = pure $ app (Variable "matrix") args

--------------------------------------------------
----            Indexation
--------------------------------------------------
indexCompute :: FormulaPrim -> [FormulaPrim] -> EqContext FormulaPrim
indexCompute a [] = return a
indexCompute n@(CInteger _) idx = eqPrimFail (indexes n idx) Err.integer_not_indexable
indexCompute n@(CFloat _) idx = eqPrimFail (indexes n idx) Err.float_not_indexable

indexCompute mm@(Matrix _ 1 m lst) idxs@(CInteger i : rest)
    | i >= 1 && m >= fromInteger i = indexCompute (lst !! (fromInteger i - 1) !! 0) rest
    | otherwise = eqPrimFail (indexes mm idxs) Err.out_of_bound_index

indexCompute mm@(Matrix _ n 1 lst) idxs@(CInteger i : rest)
    | i >= 1 && n >= fromInteger i = indexCompute (lst !! 0 !! (fromInteger i - 1)) rest
    | otherwise = eqPrimFail (indexes mm idxs) Err.out_of_bound_index

indexCompute mm@(Matrix _ n m lst) idxs@(CInteger i : CInteger j : rest)
    | i >= 1 && i <= toInteger n && j >= 1 && j <= toInteger m = 
            indexCompute (lst !! (fromInteger i - 1) !! (fromInteger j - 1)) rest
    | otherwise = eqPrimFail (indexes mm idxs) (Err.out_of_bound_index  ++ "n:" ++ show n ++ " m:" ++ show m)

indexCompute m@(Matrix _ n _ lst) idx@[CInteger i]
    | i >= 1 && i <= toInteger n = return . list $ lst !! (fromInteger i - 1)
    | otherwise = eqPrimFail (indexes m idx) Err.out_of_bound_index

indexCompute l@(List _ lst) idx@(CInteger i : rest)
    | i >= 1 && i - 1 < toInteger (length lst) = indexCompute (lst !! (fromInteger i - 1)) rest
    | otherwise = eqPrimFail (indexes l idx) Err.out_of_bound_index

indexCompute a b = return $ indexes a b

--------------------------------------------------
----            Cons evaluation
--------------------------------------------------
consEval :: EvalOp
consEval (List _ lst) toAppend = left $ list (toAppend : lst)
consEval l toAppend = 
    eqPrimFail (binOp OpCons [toAppend, l]) Err.eval_not_list >>= left

-----------------------------------------------
----        General evaluation
-----------------------------------------------
-- | General evaluation/reduction function
eval :: EvalFun -> EvalFun
eval evaluator (Meta _ m f) = metaEvaluation evaluator m f
eval evaluator (Matrix _ n m mlines) = do
    cells <- sequence [mapM evaluator line | line <- mlines]
    return $ matrix n m cells
eval evaluator (List _ l) = do list <$> mapM evaluator l
eval _ func@(Lambda _ _) = unTagFormula <$> inject (Formula func)
eval _ (Variable v) = do
    symbol <- symbolLookup v
    case symbol of
         Nothing -> return $ Variable v
         Just (Formula (f)) -> return f

eval evaluator (App _ (Variable "matrix") args) =
    mapM evaluator args >>= matrixCreate

eval evaluator fullApp@(App _ def var) = do
    redDef <- evaluator def
    redVar <- mapM evaluator var
#ifdef _DEBUG
    addTrace ("Appbegin |", treeIfyFormula . Formula $ app redDef redVar)
#endif
    needApply redDef redVar
   where needApply :: FormulaPrim -> [FormulaPrim] -> EqContext FormulaPrim
         needApply (Lambda _ funArgs) args' =
           case getFirstUnifying funArgs args' of
                Nothing -> eqPrimFail (app def var) Err.app_no_applygindef
                Just (body, subst) -> do
                    pushContext
                    addSymbols [ (name, Formula formula) 
                                        | (name, formula) <- subst]
#ifdef _DEBUG
                    addTrace ("subst | " ++ show subst, treeIfyFormula $ Formula body)
#endif
                    depth <- contextStackSize
                    if depth > maxRecursiveDepth
                        then eqFail (treeIfyFormula $ Formula fullApp) Err.max_recursion 
                          >>= return . unTagFormula
                        else do
                          injectedBody <- inject $ Formula body
                          popContext
                          body' <- evaluator $ unTagFormula injectedBody
#ifdef _DEBUG
                          addTrace ("body' | " ++ show body', treeIfyFormula $ Formula body')
#endif
                          return body'
         needApply def' args =
             return $ app def' args

eval evaluator (BinOp _ OpAdd fs) =
    binEval OpAdd (add evaluator) (add evaluator) =<< mapM evaluator fs
eval evaluator (BinOp _ OpSub fs) =
    binEval OpSub (sub evaluator) (add evaluator) =<< mapM evaluator fs
eval evaluator (BinOp _ OpMul fs) =
    binEval OpMul (mul evaluator) (mul evaluator) =<< mapM evaluator fs
eval evaluator (BinOp _ OpCons fs) =
    binEval OpCons consEval consEval =<< mapM evaluator fs

-- | Todo fix this, it's incorrect
eval evaluator (BinOp _ OpPow fs) = binEval OpPow power power =<< mapM evaluator fs
eval evaluator (BinOp _ OpDiv fs) =
    binEval OpDiv (division evaluator) (mul evaluator) =<< mapM evaluator fs

-- comparisons operators
eval evaluator (BinOp _ OpLt fs) = predicateList OpLt (compOperator (<)) =<< mapM evaluator fs
eval evaluator (BinOp _ OpGt fs) = predicateList OpGt (compOperator (>)) =<< mapM evaluator fs
eval evaluator (BinOp _ OpLe fs) = predicateList OpLe (compOperator (<=)) =<< mapM evaluator fs
eval evaluator (BinOp _ OpGe fs) = predicateList OpGe (compOperator (>=)) =<< mapM evaluator fs

eval evaluator (BinOp _ OpNe fs) = mapM evaluator fs >>= inequality
eval evaluator (BinOp _ OpEq lst) = mapM evaluator lst >>= equality

eval evaluator (BinOp _ OpAnd fs) = binEval OpAnd binand binand =<< mapM evaluator fs
eval evaluator (BinOp _ OpOr fs) = binEval OpOr binor binor =<< mapM evaluator fs

-- | Special case for programs, don't evaluate left :]
eval evaluator (BinOp _ OpAttrib [a,b]) =
    binOp OpAttrib . (a:) . (:[]) <$> evaluator b

eval _ f@(BinOp _ OpAttrib _) = eqPrimFail f Err.attrib_in_expr 

eval evaluator (UnOp _ OpFactorial f) = factorial =<< evaluator f
eval evaluator (UnOp _ OpFloor f) = floorEval =<< evaluator f
eval evaluator (UnOp _ OpCeil f) = ceilEval =<< evaluator f
eval evaluator (UnOp _ OpFrac f) = fracEval =<< evaluator f
eval evaluator (UnOp _ OpMatrixWidth f) = matrixWidthEval =<< evaluator f
eval evaluator (UnOp _ OpMatrixHeight f) = matrixHeightEval =<< evaluator f

eval evaluator (UnOp _ OpNegate f) = fNegate =<< evaluator f
eval evaluator (UnOp _ OpAbs f) = fAbs =<< evaluator f

eval evaluator (UnOp _ op f) = return . unOp op =<< evaluator f

eval evaluator f@(Derivate _ what varSpec) = do
    var'<- metaFilter evaluator varSpec 
    what' <- metaFilter evaluator what
    derivator what' var'
        where derivator toDeriv (Variable v) = do
#ifdef _DEBUG
                    addTrace ("Derivation on " ++ v, treeIfyFormula . Formula $ toDeriv)
#endif
                    derived <- derivateFormula v $ Formula toDeriv 
                    return . unTagFormula $ cleanup derived
              derivator _ _ = eqPrimFail f Err.deriv_bad_var_spec
        
eval evaluator (Indexes _ what lst) = do
    what' <- evaluator what
    lst' <- mapM evaluator lst
    indexCompute what' lst'

eval evaluator formu@(Sum _ (BinOp _ OpEq [Variable v, inexpr]) endexpr f) = do
    inexpr' <- evaluator inexpr
    endexpr' <- evaluator endexpr
    sumEval inexpr' endexpr'
     where sumEval (CInteger initi) (CInteger endi)
            | initi <= endi = iterateFormula evaluator (binOp OpAdd) v initi endi f
            | otherwise = eqPrimFail formu Err.sum_wrong_bounds
           sumEval ini end = return $ summ (binOp OpEq [Variable v, ini]) end f
    

eval evaluator formu@(Product _ (BinOp _ OpEq [Variable v, inexpr]) endexpr f) = do
    inexpr' <- evaluator inexpr
    endexpr' <- evaluator endexpr
    prodEval inexpr' endexpr'
     where prodEval (CInteger initi) (CInteger endi)
            | initi <= endi = iterateFormula evaluator (binOp OpMul) v initi endi f
            | otherwise = eqPrimFail formu Err.sum_wrong_bounds
           prodEval ini end = return $ productt (binOp OpEq [Variable v, ini]) end f
    
eval _ f@(Integrate _ _ _ _ _) =
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
matrixScalar evaluator op s m@(Matrix _ _ _ _) = matrixScalar evaluator op m s
matrixScalar evaluator op (Matrix _ n m mlines) s = matrix n m <$> cell
    where cell = sequence
            [ mapM (evaluator . (`op` s)) line | line <- mlines]
matrixScalar _ _ _ _ = error Err.matrixScalar_badop

-- | Multiplication between two matrix. Check for matrix sizes.
matrixMatrixMul :: EvalFun -> EvalOp
matrixMatrixMul evaluator m1@(Matrix _ n _ mlines) m2@(Matrix _ _n' m' mlines')
    | n /= m' = do _ <- eqFail (Formula $ binOp OpMul [m1, m2]) Err.matrix_mul_bad_size
                   right (m1, m2)
    | otherwise = cellLine >>= left . matrix n m'
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
matrixMatrixSimple evaluator op m1@(Matrix _ n m mlines) m2@(Matrix _ n' m' mlines')
    | n /= n' || m /= m' = do
        _ <- eqFail (Formula $ m1 `op` m2) Err.matrix_diff_size
        return $ Right (m1, m2)
    | otherwise = Left . matrix n m <$> newCells
        where dop (e1, e2) = evaluator $ e1 `op`e2
              newCells = sequence [ mapM dop $ zip line1 line2
                                     | (line1, line2) <- zip mlines mlines']
matrixMatrixSimple _ _ _ _ = error $ Err.shouldnt_happen "matrixMatrixSimple"

