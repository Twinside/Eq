{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module Language.Eq.Polynome( convertToPolynome
                        , convertToFormula
                        , polynomizeFormula
                        , polyMap
                        , polyCoeffMap 
                        , scalarToCoeff
                        , coefToFormula 
                        , isCoeffNull 
                        , isBelowZero
                        , prepareFormula 
                        , syntheticDiv 
                        , polyAsFormula 

                        -- | Pack/simplify polynome with only one coefficient
                        -- and/or null coef.
                        , simplifyPolynome 
                        ) where
import Data.Maybe( fromMaybe )
import Data.Ord( comparing )
import Control.Applicative( (<$>), (<*>) )
import Control.Arrow( (***), second )
import Control.Monad( join )
import Data.Either( partitionEithers )
import Data.List( sortBy, groupBy, foldl' )
import Data.Ratio

import Language.Eq.Types
import Language.Eq.Algorithm.Utils
import Language.Eq.FormulaIterator
import qualified Language.Eq.ErrorMessages as Err

-- | will pack/simplify internal representation of a polynome.
-- If there is only one null coefficient only subPoly will be present
simplifyPolynome :: Polynome -> Polynome
simplifyPolynome (Polynome v p@[(lastCoeff, PolyRest constant)])
    | isCoeffNull lastCoeff = PolyRest constant
    | otherwise = Polynome v p
simplifyPolynome (Polynome v p@[(lastCoeff, subPoly)])
    | isCoeffNull lastCoeff = subPoly
    | otherwise = Polynome v p
simplifyPolynome a = a

polyAsFormula :: Polynome -> FormulaPrim
polyAsFormula (PolyRest coeff) = coefToFormula coeff
polyAsFormula (Polynome _ [(0, a)]) = polyAsFormula a
polyAsFormula p = poly p

-- | Given a formula, it'll try to convert it to a polynome.
-- Formula should be expanded and in list form to get this
-- function to work (nested shit shouldn't work)
convertToPolynome :: Formula ListForm -> Maybe Polynome
convertToPolynome (Formula f) = polynomize 
                              $ prepareFormula f

convertToFormula :: Polynome -> Formula ListForm
convertToFormula = Formula . convertToFormulaPrim

-- | Run across the whole formula and replace what
-- can polynomized by a polynome
polynomizeFormula :: Formula ListForm -> Formula ListForm
polynomizeFormula (Formula f) = Formula $ topDownTraversal converter f
        where converter f' = poly <$> convertToPolynome (Formula f')

-- | Convert a polynome into a simpler formula using only
-- basic operators.
convertToFormulaPrim :: Polynome -> FormulaPrim
convertToFormulaPrim (PolyRest coeff) = coefToFormula coeff
convertToFormulaPrim (Polynome var lst) =
 foldl' constructor realFirst rest
    where constructor a (Left b) = a + b
          constructor a (Right b) = a - b

          realFirst = either id id felem
          (felem : rest) = map elemConverter lst

          fvar = Variable var
          elemConverter (degree,def) =
              degreeOf (convertToFormulaPrim def)
                       (coefToFormula degree)

          degreeOf            fdef (CInteger 0)
              | isConstantNegative fdef = Right $ negateConstant fdef
              | otherwise = Left $ fdef
              
          degreeOf (CInteger   1 ) (CInteger 1) = Left fvar
          degreeOf (CInteger (-1)) (CInteger 1) = Right fvar
          degreeOf fdef         (CInteger 1)
              | isConstantNegative fdef = Right $ negateConstant fdef * fvar
              | otherwise = Left $ fdef * fvar

          degreeOf (CInteger 1) deg = Left $ fvar ** deg
          degreeOf (CInteger (-1)) deg = Right $ fvar ** deg

          degreeOf fdef deg
              | isConstantNegative fdef =
                    Right $ negateConstant fdef * (fvar ** deg)
              | otherwise = Left $ fdef * (fvar ** deg)

-- | Conversion from coef to basic formula. ratio
-- are converted to (a/b), like a division.
coefToFormula :: PolyCoeff -> FormulaPrim
coefToFormula (CoeffFloat f) = CFloat f
coefToFormula (CoeffInt i) = CInteger i
coefToFormula (CoeffRatio r) = if denominator r == 1
        then CInteger $ numerator r
        else Fraction r

-- | Flatten the formula, remove all the OpSub and replace them
-- by OpAdd. Also bring lowest variables to the front, regardless of
-- their order. Ordering is very important in this function. All
-- the polynome construction is built uppon the ordering created here.
prepareFormula :: FormulaPrim -> FormulaPrim
prepareFormula = polySort . formulaFlatter

polySort :: FormulaPrim -> FormulaPrim
polySort = depthFormulaPrimTraversal `asAMonad` sortBinOp sorter
    where lexicalOrder EQ b = b
          lexicalOrder a _ = a

          invert LT = GT
          invert EQ = EQ
          invert GT = LT

          -- Special sort which bring x in front, followed by others. Lexical
          -- order first.

          sorter (Poly _ p1) (Poly _ p2) = compare p1 p2
          sorter (Poly _ _) _ = LT
          sorter _ (Poly _ _) = GT

          -- Rules to fine-sort '*' elements
          -- (x before y), no regard for formula degree
          sorter (Variable v1) (Variable v2) = compare v1 v2

          -- x ^ n * y ^ n (n can be one, not shown)
          sorter (BinOp _ OpPow [Variable v1, p1])
                 (BinOp _ OpPow [Variable v2, p2]) =
                     compare v1 v2 `lexicalOrder` compare p1 p2

          -- x * y ^ n
          sorter (Variable v1)
                 (BinOp _ OpPow (Variable v2:_)) =
                     compare v1 v2 `lexicalOrder` LT

          -- x ^ n * y
          sorter (BinOp _ OpPow (Variable v1:_))
                 (Variable v2) = compare v1 v2 `lexicalOrder` GT

          -- (x * ...) + y ^ n
          sorter (BinOp _ OpMul (Variable v1:_))
                 (BinOp _ OpPow [Variable v2, _]) = compare v1 v2 `lexicalOrder` LT

          -- x ^ n + (y * ...)
          sorter (BinOp _ OpPow [Variable v1, _])
                 (BinOp _ OpMul (Variable v2:_))  = compare v1 v2 `lexicalOrder` GT

          -- (x ^ m * ...) + y ^ n
          sorter (BinOp _ OpMul (BinOp _ OpPow [Variable v1,p1]:_))
                 (BinOp _ OpPow [Variable v2, p2]) =
                     compare v1 v2 `lexicalOrder` compare p1 p2

          -- x ^ n + (y ^ m * ...)
          sorter (BinOp _ OpPow [Variable v1, p1])
                 (BinOp _ OpMul (BinOp _ OpPow [Variable v2,p2]:_)) =
                     compare v1 v2 `lexicalOrder` compare p1 p2

          -- Rules to fine sort the '+' elements, lowest variable
          -- first (x before y), smallest order first (x before x ^ 15)

          -- (x^n * ....) + (y^n * ...)
          sorter (BinOp _ OpMul (BinOp _ OpPow (Variable v1: power1):_))
                 (BinOp _ OpMul (BinOp _ OpPow (Variable v2: power2):_)) = 
                    compare v1 v2 `lexicalOrder` compare power1 power2

          -- (x * ...) + (y^n * ...)
          sorter (BinOp _ OpMul (Variable v1:_))
                 (BinOp _ OpMul (BinOp _ OpPow (Variable v2:_):_)) =
                     compare v1 v2 `lexicalOrder` LT

          -- (x^n * ...) + (y * ...)
          sorter (BinOp _ OpMul (BinOp _ OpPow (Variable v1:_):_))
                 (BinOp _ OpMul (Variable v2:_)) = compare v1 v2 `lexicalOrder` GT

          -- (x * ...) + (y * ...)
          sorter (BinOp _ OpMul (Variable v1:_))
                 (BinOp _ OpMul (Variable v2:_)) = compare v1 v2

          -- x + (y * ...)
          sorter (Variable v1)
                 (BinOp _ OpMul (Variable v2:_)) = compare v1 v2

          -- (x * ...) + y
          sorter (BinOp _ OpMul (Variable v1:_))
                 (Variable v2) = compare v1 v2

          sorter (BinOp _ OpPow a) (BinOp _ OpPow b) =
                case comparing length a b of
                     LT -> LT
                     GT -> GT
                     EQ -> foldl' (\acc (a', b') -> if acc == EQ
                                                        then acc
                                                        else compare a' b') EQ $ zip a b
          -- x ^ n * ?
          sorter _ (BinOp _ OpPow (Variable _:_)) = GT
          sorter (BinOp _ OpPow (Variable _:_)) _ = LT

          -- make sure weird things go at the end.
          sorter (Variable _) _ = LT
          sorter _ (Variable _) = GT

          -- Just reverse the general readable order.
          sorter a b = invert $ compare a b

-- | Called when we found an OpSub operator within the
-- formula.  -- We assume that the formula as been previously sorted
resign :: FormulaPrim -> [FormulaPrim] -> [FormulaPrim]
resign = globalResign
    where globalResign (BinOp _ OpMul (a:xs)) acc
            | isFormulaInteger a = case atomicResign a of
                        Nothing -> binOp OpMul (CInteger (-1):a:xs) : acc
                        Just a' -> binOp OpMul (a':xs) : acc
          globalResign (BinOp _ OpAdd lst) acc = foldr resign acc lst
          globalResign a acc = fromMaybe (CInteger (-1) * a) (atomicResign a) : acc

          atomicResign (CInteger i) = Just $ CInteger (-i)
          atomicResign (CFloat i) = Just $ CFloat (-i)
          atomicResign (UnOp _ OpNegate a) = Just a
          atomicResign (BinOp _ OpDiv [a,b]) = (\a' -> binOp OpDiv [a', b]) <$> atomicResign a
          atomicResign _ = Nothing

-- | Flatten a whole formula, by flattening from the leafs.
formulaFlatter :: FormulaPrim -> FormulaPrim
formulaFlatter = depthFormulaPrimTraversal `asAMonad` listFlatter

-- | Given a formula in LIST form, provide a version
-- with only Pluses.
listFlatter :: FormulaPrim -> FormulaPrim
listFlatter (BinOp _ OpAdd lst) = binOp OpAdd $ foldr flatter [] lst
    where flatter (BinOp _ OpSub (x:xs)) acc = x : foldr resign acc xs
          flatter (BinOp _ OpAdd lst') acc = lst' ++ acc
          flatter x acc = x:acc
listFlatter (BinOp _ OpSub ((BinOp _ OpAdd lst'):xs)) =
    binOp OpAdd $ lst' ++ foldr resign [] xs
listFlatter (BinOp _ OpSub (x:xs)) =
    binOp OpAdd $ x : foldr resign [] xs

-- Remove the maximum of negation in the multiplication.
-- In the end, keep the needed negation into the first term
listFlatter (BinOp _ OpMul lst) = if foldr countInversion False lst
                then let (x:xs) = map cleanSign lst
                     in binOp OpMul $ resign x xs
                else binOp OpMul $ map cleanSign lst
   where iodd :: Int -> Bool
         iodd = odd
         countInversion whole@(UnOp _ OpNegate _) acc =
             if iodd . fst $ getUnsignedRoot 0 whole
                then not acc
                else acc
         countInversion _ acc = acc

         getUnsignedRoot n (UnOp _ OpNegate something) = getUnsignedRoot (n+1) something
         getUnsignedRoot n (something) = (n :: Int, something)

         cleanSign whole@(UnOp _ OpNegate _) = snd $ getUnsignedRoot 0 whole
         cleanSign a = a

listFlatter a = a

-- | Verify if the coefficient is valid in the context
-- of polynomial. might add a reduction rule here.
evalCoeff :: [FormulaPrim] -> Maybe PolyCoeff
evalCoeff [CInteger i] = Just $ CoeffInt i
evalCoeff [CFloat f] = Just $ CoeffFloat f
evalCoeff [UnOp _ OpNegate (CInteger i)] = Just $ CoeffInt (-i)
evalCoeff [UnOp _ OpNegate (CFloat f)] = Just $ CoeffFloat (-f)
evalCoeff [BinOp _ OpDiv [CInteger a, CInteger b]] = Just . CoeffRatio $ a % b
evalCoeff [UnOp _ OpNegate (BinOp _ OpDiv [CInteger a, CInteger b])] = Just . CoeffRatio $ (-a) % b
evalCoeff _ = Nothing

-- | Given a rest (a leading +c, where c is a constant) and
-- a group of variable and coefficients, try to build a full
-- blown polynomial out of it.
translator :: [FormulaPrim]                            -- Unnammed rest (var ^ 0)
           -> [(String, [(FormulaPrim, FormulaPrim)])] -- Named things x ^ n or y ^ n, n > 0
           -> Maybe (Maybe Polynome)                   -- ^ First maybe: error, nested maybe: empty
translator [] [(var, coefs)] = do 
        result <- mapM (\(rank, polyn) -> (,) <$> evalCoeff [rank] <*> polynomize polyn) coefs
        return . Just $ Polynome var result

translator pow0 [(var, coefs)] = do
        result <- mapM (\(rank,polyn) -> (,) <$> evalCoeff [rank] <*> polynomize polyn) coefs
        rest <- evalCoeff pow0
        return . Just . Polynome var $ (CoeffInt 0, PolyRest rest):result

translator pow0 ((var,coefs):rest) = do
    result <- mapM (\ (rank,polyn) -> (,) <$> evalCoeff [rank] <*> polynomize polyn) coefs
    subPolynome <- translator pow0 rest
    let finalList = case subPolynome of
                         Nothing -> result
                         Just p -> (CoeffInt 0, p) : result
    return . Just $ Polynome var finalList

translator pow0 [] = return $ PolyRest <$> evalCoeff pow0

-- | Try to transform a formula in polynome.
polynomize :: FormulaPrim -> Maybe Polynome
polynomize wholeFormula@(BinOp _ OpMul _) = polynomize (binOp OpAdd [wholeFormula])
-- HMmm?
polynomize (BinOp _ OpAdd lst) = join             -- flatten a maybe level, we don't distingate
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
                                 , [(coef group, polySort $ binOp OpAdd $ defs group) 
                                                | group <- coeffGroup lst'])
                  defs = map (\(_,_,def) -> def)
                  coef ((_,c1,_):_) = c1
                  coef [] = error Err.polynom_emptyCoeffPack

polynomize (BinOp _ OpPow [Variable v, CInteger c]) =
        Just $ Polynome v [(CoeffInt c, PolyRest 1)]
polynomize _ = Nothing

-- | Function in charge of extracting variable name (if any), and
-- return the coeff function.
extractFirstTerm :: FormulaPrim
                 -> Either (String, FormulaPrim, FormulaPrim) FormulaPrim
extractFirstTerm fullFormula@(BinOp _ OpMul lst) = varCoef lst
    where varCoef ((BinOp _ OpPow [(Variable v), f]):xs)
                | isFormulaConstant f = Left (v, f, multify xs)
          varCoef ((Variable v):xs) = Left (v, CInteger 1, multify xs)
          varCoef _ = Right fullFormula
        
          multify [] = error $ Err.empty_binop "Polynome.OpMul"
          multify [x] = x
          multify alist = binOp OpMul alist

extractFirstTerm (BinOp _ OpPow [Variable v, order])
    | isFormulaConstant order = Left (v, order, CInteger 1)

extractFirstTerm (Variable v) = Left (v, CInteger 1, CInteger 1)

extractFirstTerm a = Right a

--------------------------------------------------
----            Polynome instances
--------------------------------------------------

-- | Only to map on the polynome coefficients (not the degree
-- of it).
polyCoeffMap :: (PolyCoeff -> PolyCoeff) -> Polynome -> Polynome
polyCoeffMap f = polyMap mapper
    where mapper (deg, PolyRest c) = (deg, PolyRest $ f c)
          mapper otherCoeff = otherCoeff

-- | polynome mapping
polyMap :: ((PolyCoeff, Polynome) -> (PolyCoeff, Polynome)) -> Polynome -> Polynome
polyMap f (Polynome s lst) = Polynome s $ map (second $ polyMap f) lst
polyMap f rest@(PolyRest _) = snd $ f (CoeffInt 0, rest)

-- | Transform a scalar formula component to
-- a polynome coefficient. If formula is not
-- a scalar, error is called.
scalarToCoeff :: FormulaPrim -> PolyCoeff
scalarToCoeff (UnOp _ OpNegate f) = negate $ scalarToCoeff f
scalarToCoeff (CFloat f) = CoeffFloat f
scalarToCoeff (CInteger i) = CoeffInt i
scalarToCoeff (BinOp _ OpDiv [CInteger a, CInteger b]) = CoeffRatio $ a % b
scalarToCoeff _ = error Err.polynom_coeff_notascalar

-- | Operation on polynome coefficients. Put there
-- to provide automatic Equality derivation for polynome
-- and in the end... Formula
coeffOp :: (forall a. (Num a) => a -> a -> a)
        -> PolyCoeff -> PolyCoeff -> PolyCoeff
coeffOp op c1 c2 = eval $ polyCoeffCast c1 c2
    where eval (CoeffInt i1, CoeffInt i2) = CoeffInt $ i1 `op` i2
          eval (CoeffFloat f1, CoeffFloat f2) = CoeffFloat $ f1 `op` f2
          eval (CoeffRatio r1, CoeffRatio r2) = CoeffRatio $ r1 `op` r2
          eval _ = error Err.polynom_bad_casting 

inf :: PolyCoeff -> PolyCoeff -> Bool
inf = coeffPredicate ((<) :: forall a. (Ord a) => a -> a -> Bool)

-- | Implement the same idea that the one used by the
-- mergesort, only this time it's only used to perform
-- addition or substraction on polynomial.
lockStep :: (Polynome -> Polynome -> Polynome)
         -> [(PolyCoeff, Polynome)] -> [(PolyCoeff, Polynome)]
         -> [(PolyCoeff, Polynome)]
lockStep op xs [] = map (\(c,v) -> (c, v `op` PolyRest 0)) xs
lockStep op [] ys = map (\(c,v) -> (c, PolyRest 0 `op` v)) ys
lockStep op whole1@((c1, def1):xs) whole2@((c2, def2):ys)
    | c1 `inf` c2 = 
        (c1, def1 `op` PolyRest (CoeffInt 0)) : lockStep op xs whole2
    | c1  ==   c2 = 
        (c1, def1 `op` def2) : lockStep op xs ys
    | otherwise   =
        (c2, PolyRest (CoeffInt 0) `op` def2) : lockStep op whole1 ys

isBelowZero :: PolyCoeff -> Bool
isBelowZero (CoeffInt r) = r < 0
isBelowZero (CoeffFloat r) = r < 0
isBelowZero (CoeffRatio r) = r < 0
isBelowZero _ = False

-- | Tell if a coefficient can be treated as Null
isCoeffNull :: PolyCoeff -> Bool
isCoeffNull (CoeffInt 0) = True
isCoeffNull (CoeffFloat 0.0) = True
isCoeffNull (CoeffRatio r) = numerator r == 0
isCoeffNull _ = False

coeffPropagator :: (forall a. (Num a) => a -> a -> a) -> (PolyCoeff, Polynome) -> (PolyCoeff, Polynome)
coeffPropagator op (degree, PolyRest a) = (degree, PolyRest $ coeffOp op (CoeffInt 0) a)
coeffPropagator op (degree, Polynome v lst) = (degree, Polynome v $ map (coeffPropagator op) lst)


polySimpleOp :: (forall a. (Num a) => a -> a -> a) -> Polynome -> Polynome -> Polynome
polySimpleOp _ (Polynome _ []) _ = error Err.ill_formed_polynomial
polySimpleOp _ _ (Polynome _ []) = error Err.ill_formed_polynomial

polySimpleOp op (PolyRest c1) (PolyRest c2) = PolyRest $ coeffOp op c1 c2

polySimpleOp op left@(PolyRest c1) (Polynome v1 as@((coeff, def):xs))
    | isCoeffNull coeff = case def of
        PolyRest a -> Polynome v1 $ (CoeffInt 0, PolyRest $ coeffOp op c1 a) : map (coeffPropagator op) xs
        _          -> Polynome v1 $ (coeff,polySimpleOp op left def) : map (coeffPropagator op) xs

    | otherwise = 
        Polynome v1 $ (CoeffInt 0, PolyRest $ coeffOp op c1 (CoeffInt 0)) : map (coeffPropagator op) as

polySimpleOp op (Polynome v1 as@((coeff, def):xs)) right@(PolyRest c1)
    | isCoeffNull coeff = case def of
        PolyRest a -> Polynome v1 $ (CoeffInt 0, PolyRest $ coeffOp op a c1) 
                                  : map (coeffPropagator $ flip op) xs
        _          -> Polynome v1 $ (coeff,polySimpleOp op def right) 
                                  : map (coeffPropagator $ flip op) xs
    | otherwise = 
        Polynome v1 $ (CoeffInt 0, PolyRest $ coeffOp op (CoeffInt 0) c1) 
                    : as

polySimpleOp op (Polynome v1 as@((c, d1):rest)) right@(Polynome v2 bs)
    | v1 > v2 = polySimpleOp (flip op) (Polynome v2 bs) (Polynome v1 as)
    | v1 == v2 =
        let computedCoefs = lockStep op as bs
        in if null computedCoefs then PolyRest 0
                                 else Polynome v1 computedCoefs 
    | isCoeffNull c = 
        Polynome v1 $ (c, polySimpleOp op d1 right) : map (coeffPropagator $ flip op) rest

    | otherwise = 
        Polynome v1 $ (CoeffInt 0, polySimpleOp op (PolyRest $ CoeffInt 0) right)
                    : map (coeffPropagator $ flip op) as


-- | Multiply two polynomials between them using the brute force
-- way, algorithm in O(n²)
polyMul :: Polynome -> Polynome -> Polynome
polyMul p@(Polynome _ _) (PolyRest c) = polyCoeffMap (* c) p
polyMul (PolyRest c) p@(Polynome _ _) = polyCoeffMap (c *) p
polyMul (PolyRest c) (PolyRest c2) = PolyRest $ coeffOp (*) c c2
polyMul p1@(Polynome v1 _) p2@(Polynome v2 _) | v1 > v2 = polyMul p2 p1
polyMul (Polynome v1 coefs1) p2@(Polynome v2 coefs2)
    | v1 /= v2 {- v1 < v2 by previous line -} =
        Polynome v1 $ map (\(order, c) -> (order, polyMul c p2)) coefs1
    | otherwise {- v1 == v2 -} =
        Polynome v1
      {-. map (\lst@((o,_):_) -> (o, foldr1 (+) $ map snd lst))-}
      . map headSum
      . groupBy (\(o1,_) (o2,_) -> o1 == o2) -- Regroup same order together
      $ sortBy (\(c1,_) (c2,_) -> compare c1 c2)
      [ (degree1 + degree2, c1 * c2) | (degree1, c1) <- coefs1, (degree2, c2) <- coefs2]
        where headSum lst@((o,_):_) = (o, sum $ map snd lst)
              headSum [] = error "Polynome.hs - headSum - error Empty list"

--------------------------------------------------
----            Division
--------------------------------------------------
-- | Expand coefficients of an _UNIVARIATE_ polynomial
-- in an descending way, each integer power given a
-- coefficient (0 if none).
expandCoeff :: Polynome -> Maybe [PolyCoeff]
expandCoeff (PolyRest _) = error ""
expandCoeff (Polynome _ coefs) = snd <$> foldl' sparser (Just (-1, [])) coefs
    where sparser (Just (lastNum, lst)) (CoeffInt n, PolyRest r) =
              Just (fromInteger n, r : replicate (fromInteger n - lastNum - 1) (CoeffInt 0)
                                    ++ lst)
          sparser _ _ = Nothing

-- | Tell if a polynomial has only one var
isPolyMonovariate :: Polynome -> Bool
isPolyMonovariate (PolyRest _) = False
isPolyMonovariate (Polynome _ coefs) = all isCoeff coefs
    where isCoeff (_,PolyRest _) = True
          isCoeff              _ = False

-- | Given a power descending list of coefficient, rearrange
-- them to make it normal polynomial
packCoeffs :: [PolyCoeff] -> [(PolyCoeff, Polynome)]
packCoeffs = reverse . snd . foldr packer (0, [])
    where packer coeff (n, lst)
            | isCoeffNull coeff = (n + 1, lst)
            | otherwise = (n + 1, (CoeffInt n, PolyRest coeff) : lst)

-- | Apply an operation on an head of a list given an other list.
-- return Nothing if first list finish after "applied" list.
headApply :: (a -> b -> a) -> [a] -> [b] -> Maybe [a]
headApply _     []     [] = Just []
headApply _   rest     [] = Just rest
headApply _     []      _ = Nothing
headApply f (x:xs) (y:ys) = (f x y :) <$> headApply f xs ys

-- | Try to perform a polynomial synthetic division on
-- monovariate polynomial.
syntheticDiv :: Polynome -> Polynome -> (Maybe Polynome, Maybe Polynome)
syntheticDiv polyn@(Polynome var lst1) divisor@(Polynome var' lst2)
    | var == var'
    && isPolyMonovariate polyn && isPolyMonovariate divisor
    && fst (last lst1) > fst (last lst2) =

        (finalize . packCoeffs . map (/ normalizingCoeff)
            *** finalize . packCoeffs)

      . splitAt (length coefList + 1 - length divCoeff)
      $ firstCoeff : syntheticInnerDiv divCoeff firstCoeff coefList

    where Just (firstCoeff: coefList) = expandCoeff polyn
          Just (firstDivCoeff:divCoeff) = map negate <$> expandCoeff divisor

          normalizingCoeff = negate firstDivCoeff

          finalize [] = Nothing
          finalize lst = Just $ Polynome var lst

          syntheticInnerDiv :: [PolyCoeff]
                            -> PolyCoeff -> [PolyCoeff] -> [PolyCoeff]
          syntheticInnerDiv         _         _        [] = []
          syntheticInnerDiv diviCoeff prevCoeff polyCoeff =
            case endCoeffs of
                   Just [] -> error "syntheticDiv - empty rest, impossible"
                   Just (x:xs) -> x : syntheticInnerDiv diviCoeff x xs
                   Nothing -> polyCoeff
              where normalizedCoeff = prevCoeff / normalizingCoeff
                    endCoeffs = headApply (+) polyCoeff 
                              $ map (normalizedCoeff *) diviCoeff
syntheticDiv _ _ = (Nothing, Nothing)

instance Num PolyCoeff  where
    fromInteger = CoeffInt
    (+)  = coeffOp (+)
    (-)  = coeffOp (-)
    (*)  = coeffOp (*)

    abs (CoeffInt i) = CoeffInt $ abs i
    abs (CoeffFloat f) = CoeffFloat $ abs f
    abs (CoeffRatio r) = CoeffRatio $ abs r

    signum (CoeffInt i) = CoeffInt $ signum i
    signum (CoeffFloat f) = CoeffFloat $ signum f
    signum (CoeffRatio r) = CoeffRatio $ signum r

instance Fractional PolyCoeff where
    a / b = case polyCoeffCast a b of
        (CoeffInt i1, CoeffInt i2) -> if i1 `mod` i2 == 0
                        then CoeffInt $ i1 `div` i2
                        else CoeffRatio $ i1 % i2
        (CoeffFloat f1, CoeffFloat f2) -> CoeffFloat $ f1 / f2
        (CoeffRatio r1, CoeffRatio r2) -> CoeffRatio $ r1 / r2
        _ -> error Err.polynom_bad_casting 

    recip (CoeffFloat f) = CoeffFloat $ recip f 
    recip (CoeffInt i) = CoeffRatio $ 1 % i
    recip (CoeffRatio r) = if denominator r' == 1
                then CoeffInt $ numerator r'
                else CoeffRatio r'
        where r' = recip r

    fromRational = CoeffRatio

instance Num Polynome where
    (+) = polySimpleOp (+)
    (-) = polySimpleOp (-)
    (*) = polyMul
    fromInteger = PolyRest . fromInteger
    abs = error "Unimplemented-Abs"
    signum = error "Unimplemented-signum"

