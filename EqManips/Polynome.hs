{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module EqManips.Polynome( convertToPolynome
                        , convertToFormula
                        , polyMap
                        ) where

import Control.Applicative( (<$>), (<*>) )
import Control.Monad( join )
import Data.Either( partitionEithers )
import Data.List( groupBy, foldl' )
import Data.Ratio

import EqManips.Types
import EqManips.Algorithm.Utils
import EqManips.FormulaIterator
import qualified EqManips.ErrorMessages as Err

-- | Given a formula, it'll try to convert it to a polynome.
-- Formula should be expanded and in list form to get this
-- function to work (nested shit shouldn't work)
convertToPolynome :: Formula ListForm -> Maybe Polynome
convertToPolynome (Formula f) = polynomize $ prepareFormula f


convertToFormula :: Polynome -> Formula ListForm
convertToFormula = Formula . convertToFormulaPrim

-- | Convert a polynome into a simpler formula using only
-- basic operators.
convertToFormulaPrim :: Polynome -> FormulaPrim
convertToFormulaPrim (PolyRest coeff) = coefToFormula coeff
convertToFormulaPrim (Polynome var lst) = BinOp OpAdd $ map elemConverter lst
    where fvar = Variable var
          elemConverter (degree,def) = degreeOf (convertToFormulaPrim def) degree
          degreeOf fdef degree = case coefToFormula degree of
                   CInteger 0 -> fdef
                   CInteger 1 -> fdef * fvar
                   deg -> fdef * (fvar ** deg)

-- | Conversion from coef to basic formula. ratio
-- are converted to (a/b), like a division.
coefToFormula :: PolyCoeff -> FormulaPrim
coefToFormula (CoeffFloat f) = CFloat f
coefToFormula (CoeffInt i) = CInteger i
coefToFormula (CoeffRatio r) = if denominator r == 1
        then CInteger $ numerator r
        else (CInteger $ numerator r)
           / (CInteger $ denominator r)

-- | Flatten the formula, remove all the OpSub and replace them
-- by OpAdd. Also bring lowest variables to the front, regardless of
-- their order. Ordering is very important in this function. All
-- the polynome construction is built uppon the ordering created here.
prepareFormula :: FormulaPrim -> FormulaPrim
prepareFormula = polySort . formulaFlatter
    where polySort = depthFormulaPrimTraversal `asAMonad` (sortBinOp sorter)

          lexicalOrder EQ b = b
          lexicalOrder a _ = a

          -- Special sort which bring x in front, followed by others. Lexical
          -- order first.

          -- Rules to fine-sort '*' elements
          -- (x before y), no regard for formula degree
          sorter (Variable v1) (Variable v2) = invert $ compare v1 v2
          sorter (BinOp OpPow [Variable v1, _p1])
                 (BinOp OpPow [Variable v2, _p2]) = compare v1 v2

          sorter (Variable v1)
                 (BinOp OpPow (Variable v2:_)) = compare v1 v2
          sorter (BinOp OpPow (Variable v1:_))
                 (Variable v2) = compare v1 v2

          sorter _ (BinOp OpPow (Variable _:_)) = GT
          sorter (BinOp OpPow (Variable _:_)) _ = LT

          -- Rules to fine sort the '+' elements, lowest variable
          -- first (x before y), smallest order first (x before x ^ 15)
          sorter (BinOp OpMul (BinOp OpPow (Variable v1: power1):_))
                 (BinOp OpMul (BinOp OpPow (Variable v2: power2):_)) = 
                    (compare v1 v2) `lexicalOrder` (compare power1 power2)

          sorter (BinOp OpMul (Variable v1:_))
                 (BinOp OpMul (BinOp OpPow (Variable v2:_):_)) = (compare v1 v2) `lexicalOrder` LT

          sorter (BinOp OpMul (BinOp OpPow (Variable v1:_):_))
                 (BinOp OpMul (Variable v2:_)) = (compare v1 v2) `lexicalOrder` GT

          sorter (BinOp OpPow a) (BinOp OpPow b) =
                case compare (length a) (length b) of
                     LT -> LT
                     GT -> GT
                     EQ -> foldl' (\acc (a', b') -> if acc == EQ
                                                        then acc
                                                        else compare a' b') EQ $ zip a b
          -- make sure weird things go at the end.
          sorter (Variable _) _ = LT
          sorter _ (Variable _) = GT

          -- Just reverse the general readable order.
          sorter a b = invert $ compare a b

          invert LT = GT
          invert EQ = EQ
          invert GT = LT

-- | Called when we found an OpSub operator within the
-- formula.  -- We assume that the formula as been previously sorted
resign :: FormulaPrim -> [FormulaPrim] -> [FormulaPrim]
resign = globalResign
    where globalResign (BinOp OpMul (a:xs)) acc
            | isFormulaInteger a = case atomicResign a of
                        Nothing -> BinOp OpMul (CInteger (-1):a:xs) : acc
                        Just a' -> BinOp OpMul (a':xs) : acc
          globalResign (BinOp OpAdd lst) acc = foldr resign acc lst
          globalResign a acc = (maybe ((CInteger (-1)) * a) id (atomicResign a)) : acc

          atomicResign (CInteger i) = Just $ CInteger (-i)
          atomicResign (CFloat i) = Just $ CFloat (-i)
          atomicResign (UnOp OpNegate a) = Just a
          atomicResign (BinOp OpDiv [a,b]) = (\a' -> BinOp OpDiv [a', b]) <$> atomicResign a
          atomicResign _ = Nothing

-- | Flatten a whole formula, by flattening from the leafs.
formulaFlatter :: FormulaPrim -> FormulaPrim
formulaFlatter = depthFormulaPrimTraversal `asAMonad` listFlatter

-- | Given a formula in LIST form, provide a version
-- with only Pluses.
listFlatter :: FormulaPrim -> FormulaPrim
listFlatter (BinOp OpAdd lst) = BinOp OpAdd $ foldr flatter [] lst
    where flatter (BinOp OpSub (x:xs)) acc = x : foldr resign acc xs
          flatter (BinOp OpAdd lst') acc = lst' ++ acc
          flatter x acc = x:acc
listFlatter (BinOp OpSub ((BinOp OpAdd lst'):xs)) =
    BinOp OpAdd $ lst' ++ foldr resign [] xs
listFlatter (BinOp OpSub (x:xs)) =
    BinOp OpAdd $ x : foldr resign [] xs

-- Remove the maximum of negation in the multiplication.
-- In the end, keep the needed negation into the first term
listFlatter (BinOp OpMul lst) = if foldr countInversion False lst
                then let (x:xs) = map cleanSign lst
                     in BinOp OpMul $ resign x xs
                else BinOp OpMul $ map cleanSign lst
   where iodd :: Int -> Bool
         iodd = odd
         countInversion whole@(UnOp OpNegate _) acc =
             if iodd . fst $ getUnsignedRoot 0 whole
                then not acc
                else acc
         countInversion _ acc = acc

         getUnsignedRoot n (UnOp OpNegate something) = getUnsignedRoot (n+1) something
         getUnsignedRoot n (something) = (n :: Int, something)

         cleanSign whole@(UnOp OpNegate _) = snd $ getUnsignedRoot 0 whole
         cleanSign a = a

listFlatter a = a

-- | Verify if the coefficient is valid in the context
-- of polynomial. might add a reduction rule here.
evalCoeff :: [FormulaPrim] -> Maybe PolyCoeff
evalCoeff [CInteger i] = Just $ CoeffInt i
evalCoeff [CFloat f] = Just $ CoeffFloat f
evalCoeff [UnOp OpNegate (CInteger i)] = Just $ CoeffInt (-i)
evalCoeff [UnOp OpNegate (CFloat f)] = Just $ CoeffFloat (-f)
evalCoeff [BinOp OpDiv [CInteger a, CInteger b]] = Just . CoeffRatio $ a % b
evalCoeff [UnOp OpNegate (BinOp OpDiv [CInteger a, CInteger b])] = Just . CoeffRatio $ (-a) % b
evalCoeff _ = Nothing

-- | Given a rest (a leading +c, where c is a constant) and
-- a group of variable and coefficients, try to build a full
-- blown polynomial out of it.
translator :: [FormulaPrim]                            -- Unnammed rest (var ^ 0)
           -> [(String, [(FormulaPrim, FormulaPrim)])] -- Named things x ^ n or y ^ n, n > 0
           -> Maybe (Maybe Polynome)                   -- ^ First maybe: error, nested maybe: empty
translator [] [(var, coefs)] = do 
        result <- mapM (\(rank,poly) -> (,) <$> evalCoeff [rank] <*> polynomize poly) coefs
        return . Just $ Polynome var result

translator pow0 [(var, coefs)] = do
        result <- mapM (\(rank,poly) -> (,) <$> evalCoeff [rank] <*> polynomize poly) coefs
        rest <- evalCoeff pow0
        return . Just . Polynome var $ (CoeffInt 0, PolyRest rest):result

translator pow0 ((var,coefs):rest) = do
    result <- mapM (\(rank,poly) -> (,) <$> evalCoeff [rank] <*> polynomize poly) coefs
    subPolynome <- translator pow0 rest
    let finalList = case subPolynome of
                         Nothing -> result
                         Just p -> (CoeffInt 0, p) : result
    return . Just $ Polynome var finalList

translator pow0 [] = return $ PolyRest <$> evalCoeff pow0

-- | Try to transform a formula in polynome.
polynomize :: FormulaPrim -> Maybe Polynome
polynomize wholeFormula@(BinOp OpMul _) = polynomize (BinOp OpAdd [wholeFormula])
polynomize (BinOp OpAdd lst) = join             -- flatten a maybe level, we don't distingate
                             . translator pow0  -- cases at the upper level.
                             . packCoefs
                             $ varGroup polys
  where (polys, pow0) = partitionEithers $ map extractFirstTerm lst
        varGroup = groupBy (\(var,_,_) (var',_,_) -> var == var')
        coeffGroup = groupBy (\(_,coeff1,_) (_,coeff2,_) -> coeff1 == coeff2)

        packCoefs :: [[(String,FormulaPrim,FormulaPrim)]] -> [(String, [(FormulaPrim,FormulaPrim)])]
        packCoefs varGrouped = map grouper varGrouped
            where nameOfGroup ((varName, _,_):_) = varName
                  nameOfGroup [] = error Err.polynom_emptyCoeffPack

                  grouper :: [(String,FormulaPrim,FormulaPrim)] -> (String, [(FormulaPrim,FormulaPrim)])
                  grouper lst' = (nameOfGroup lst'
                                 , [(coef group, BinOp OpAdd $ defs group) | group <- coeffGroup lst'])
                  defs = map (\(_,_,def) -> def)
                  coef ((_,c1,_):_) = c1
                  coef [] = error Err.polynom_emptyCoeffPack

polynomize _ = Nothing

-- | Function in charge of extracting variable name (if any), and
-- return the coeff function.
extractFirstTerm :: FormulaPrim
                 -> Either (String, FormulaPrim, FormulaPrim) FormulaPrim
extractFirstTerm fullFormula@(BinOp OpMul lst) = varCoef lst
    where varCoef ((BinOp OpPow [(Variable v), f]):xs)
                | isFormulaConstant f = Left (v, f, multify xs)
          varCoef ((Variable v):xs) = Left (v, CInteger 1, multify xs)
          varCoef _ = Right fullFormula
        
          multify [] = error $ Err.empty_binop "Polynome.OpMul"
          multify [x] = x
          multify alist = BinOp OpMul alist

extractFirstTerm a = Right a

--------------------------------------------------
----            Polynome instances
--------------------------------------------------

-- | polynome mapping
polyMap :: ((PolyCoeff, Polynome) -> (PolyCoeff, Polynome)) -> Polynome -> Polynome
polyMap f (Polynome s lst) = Polynome s $ map (\(c,p) -> (c, polyMap f p)) lst
polyMap f rest@(PolyRest _) = snd $ f (CoeffInt 0, rest)

-- | Little helpa fellow

coeffOp :: (forall a. (Num a) => a -> a -> a) -> PolyCoeff -> PolyCoeff -> PolyCoeff
coeffOp op c1 c2 = eval $ samerizer c1 c2
    where eval (CoeffInt i1, CoeffInt i2) = CoeffInt $ i1 `op` i2
          eval (CoeffFloat f1, CoeffFloat f2) = CoeffFloat $ f1 `op` f2
          eval (CoeffRatio r1, CoeffRatio r2) = CoeffRatio $ r1 `op` r2
          eval _ = error Err.polynom_bad_casting 

coeffPredicate :: (forall a. Ord a => a -> a -> Bool) -> PolyCoeff -> PolyCoeff -> Bool
coeffPredicate op c1 c2 = eval $ samerizer c1 c2
    where eval (CoeffInt i1, CoeffInt i2) = i1 `op` i2
          eval (CoeffFloat f1, CoeffFloat f2) = f1 `op` f2
          eval (CoeffRatio r1, CoeffRatio r2) = r1 `op` r2
          eval _ = error Err.polynom_bad_casting 

-- | Samerizer autocast to the same level
samerizer :: PolyCoeff -> PolyCoeff -> (PolyCoeff, PolyCoeff)
samerizer (CoeffInt i1) (CoeffInt i2) = (CoeffInt i1, CoeffInt i2)
samerizer (CoeffFloat f1) (CoeffFloat f2) = (CoeffFloat f1,CoeffFloat f2)
samerizer (CoeffRatio r1) (CoeffRatio r2) = (CoeffRatio r1, CoeffRatio r2)
samerizer (CoeffInt i1) (CoeffRatio r2) = (CoeffRatio $ i1 % 1, CoeffRatio r2)
samerizer (CoeffRatio r1) (CoeffInt i2) = (CoeffRatio r1, CoeffRatio $ i2 % 1)
samerizer (CoeffInt i1) (CoeffFloat f2) = (CoeffFloat $ fromInteger i1, CoeffFloat f2)
samerizer (CoeffFloat f1) (CoeffInt i2) = (CoeffFloat f1, CoeffFloat $ fromInteger i2)
samerizer (CoeffFloat f1) (CoeffRatio r2) = (CoeffFloat f1, CoeffFloat $ fromRational r2)
samerizer (CoeffRatio r1) (CoeffFloat f2) = (CoeffFloat $ fromRational r1, CoeffFloat f2)

inf :: PolyCoeff -> PolyCoeff -> Bool
inf = coeffPredicate ((<) :: forall a. (Ord a) => a -> a -> Bool)

-- | Implement the same idea that the one used by the
-- mergesort, only this time it's only used to perform
-- addition or substraction on polynomial.
lockStep :: (Polynome -> Polynome -> Polynome)
         -> [(PolyCoeff, Polynome)] -> [(PolyCoeff, Polynome)]
         -> [(PolyCoeff, Polynome)]
lockStep  _ xs [] = xs
lockStep  _ [] ys = ys
lockStep op whole1@((c1, def1):xs) whole2@((c2, def2):ys)
    | c1 `inf` c2 = (c1, def1) : lockStep op xs whole2
    | c1  ==   c2 = (c1, def1 `op` def2) : lockStep op xs ys
    | otherwise   = (c2, def2) : lockStep op whole1 ys

-- | Tell if a coefficient can be treated as Null
isCoeffNull :: PolyCoeff -> Bool
isCoeffNull (CoeffInt 0) = True
isCoeffNull (CoeffFloat 0.0) = True
isCoeffNull (CoeffRatio r) = numerator r == 0
isCoeffNull _ = False

polySimpleOp :: (forall a. (Num a) => a -> a -> a) -> Polynome -> Polynome -> Polynome
polySimpleOp _ (Polynome _ []) _ = error Err.ill_formed_polynomial
polySimpleOp _ _ (Polynome _ []) = error Err.ill_formed_polynomial
polySimpleOp op (PolyRest c1) (PolyRest c2) = PolyRest $ coeffOp (flip op) c1 c2
polySimpleOp op left@(PolyRest _) right@(Polynome _ _) = polySimpleOp (flip op) right left
polySimpleOp op (Polynome v1 as@((coeff, def):xs)) right@(PolyRest c1)
    | isCoeffNull coeff = case def of
        PolyRest a -> Polynome v1 $ (CoeffInt 0, PolyRest $ coeffOp op a c1):xs
        _          -> Polynome v1 $ (coeff,polySimpleOp op def right):xs
    | otherwise = Polynome v1 $ (CoeffInt 0, PolyRest $ coeffOp op (CoeffInt 0) c1):as

polySimpleOp op (Polynome v1 as@((c, d1):rest)) left@(Polynome v2 bs)
    | v2 > v1 = polySimpleOp op (Polynome v2 bs) (Polynome v1 as)
    | v1 == v2 = Polynome v1 $ lockStep op as bs
    | isCoeffNull c = case d1 of 
          PolyRest a   -> polyMap executor left
                where executor (c', PolyRest n) = (c', PolyRest $ coeffOp op a n)
                      executor a' = a'
          Polynome _ _ -> Polynome v1 $ (CoeffInt 0, polySimpleOp op d1 left) : rest
    | otherwise = Polynome v1 $ (CoeffInt 0, left) : as

instance Num Polynome where
    (+) = polySimpleOp (+)
    (-) = polySimpleOp (-)
    (*) = error "Unimplemented"
    abs = error "Unimplemented"
    signum = error "Unimplemented"
    fromInteger = error "Unimplemented"


