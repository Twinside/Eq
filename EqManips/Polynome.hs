{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module EqManips.Polynome( convertToPolynome
                        , convertToFormula
                        , polynomizeFormula
                        , polyMap
                        , polyCoeffMap 
                        , scalarToCoeff
                        , coefToFormula 
                        , isCoeffNull 
                        , prepareFormula 
                        ) where

import Control.Applicative( (<$>), (<*>) )
import Control.Monad( join )
import Data.Either( partitionEithers )
import Data.List( sortBy, groupBy, foldl', partition )
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

-- | Run across the whole formula and replace what
-- can polynomized by a polynome
polynomizeFormula :: Formula ListForm -> Formula ListForm
polynomizeFormula (Formula f) = Formula $ topDownTraversal converter f
        where converter f' = Poly <$> convertToPolynome (Formula f')

-- | Convert a polynome into a simpler formula using only
-- basic operators.
convertToFormulaPrim :: Polynome -> FormulaPrim
convertToFormulaPrim (PolyRest coeff) = coefToFormula coeff
convertToFormulaPrim (Polynome var lst) = adder $ map elemConverter lst
    where adder [x] = x
          adder rest = BinOp OpAdd rest
          fvar = Variable var
          elemConverter (degree,def) = degreeOf (convertToFormulaPrim def)
                                                (coefToFormula degree)

          degreeOf fdef (CInteger 0) = fdef
          degreeOf (CInteger 1) (CInteger 1) = fvar
          degreeOf fdef (CInteger 1) = fdef * fvar
          degreeOf (CInteger 1) deg = fvar ** deg
          degreeOf fdef deg = fdef * (fvar ** deg)

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

          sorter (Poly p1) (Poly p2) = compare p1 p2
          sorter (Poly _) _ = LT
          sorter _ (Poly _) = GT

          -- Rules to fine-sort '*' elements
          -- (x before y), no regard for formula degree
          sorter (Variable v1) (Variable v2) = compare v1 v2

          -- x ^ n * y ^ n (n can be one, not shown)
          sorter (BinOp OpPow [Variable v1, _p1])
                 (BinOp OpPow [Variable v2, _p2]) = compare v1 v2

          -- x * y ^ n
          sorter (Variable v1)
                 (BinOp OpPow (Variable v2:_)) = compare v1 v2

          -- x ^ n * y
          sorter (BinOp OpPow (Variable v1:_))
                 (Variable v2) = compare v1 v2

          -- (x * ...) + y ^ n
          sorter (BinOp OpMul (Variable v1:_))
                 (BinOp OpPow [Variable v2, _]) = compare v1 v2

          -- x ^ n + (y * ...)
          sorter (BinOp OpPow [Variable v1, _])
                 (BinOp OpMul (Variable v2:_))  = compare v1 v2

          -- (x ^ m * ...) + y ^ n
          sorter (BinOp OpMul (BinOp OpPow (Variable v1:_):_))
                 (BinOp OpPow [Variable v2, _]) = compare v1 v2

          -- x ^ n + (y ^ m * ...)
          sorter (BinOp OpPow [Variable v1, _])
                 (BinOp OpMul (BinOp OpPow (Variable v2:_):_))  = compare v1 v2
          -- Rules to fine sort the '+' elements, lowest variable
          -- first (x before y), smallest order first (x before x ^ 15)

          -- (x^n * ....) + (y^n * ...)
          sorter (BinOp OpMul (BinOp OpPow (Variable v1: power1):_))
                 (BinOp OpMul (BinOp OpPow (Variable v2: power2):_)) = 
                    (compare v1 v2) `lexicalOrder` (compare power1 power2)

          -- (x * ...) + (y^n * ...)
          sorter (BinOp OpMul (Variable v1:_))
                 (BinOp OpMul (BinOp OpPow (Variable v2:_):_)) =
                     (compare v1 v2) `lexicalOrder` LT

          -- (x^n * ...) + (y * ...)
          sorter (BinOp OpMul (BinOp OpPow (Variable v1:_):_))
                 (BinOp OpMul (Variable v2:_)) = (compare v1 v2) `lexicalOrder` GT

          -- (x * ...) + (y * ...)
          sorter (BinOp OpMul (Variable v1:_))
                 (BinOp OpMul (Variable v2:_)) = compare v1 v2

          -- x + (y * ...)
          sorter (Variable v1)
                 (BinOp OpMul (Variable v2:_)) = compare v1 v2

          -- (x * ...) + y
          sorter (BinOp OpMul (Variable v1:_))
                 (Variable v2) = compare v1 v2

          sorter (BinOp OpPow a) (BinOp OpPow b) =
                case compare (length a) (length b) of
                     LT -> LT
                     GT -> GT
                     EQ -> foldl' (\acc (a', b') -> if acc == EQ
                                                        then acc
                                                        else compare a' b') EQ $ zip a b
          -- x ^ n * ?
          sorter _ (BinOp OpPow (Variable _:_)) = GT
          sorter (BinOp OpPow (Variable _:_)) _ = LT

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
    result <- mapM (\ (rank,poly) -> (,) <$> evalCoeff [rank] <*> polynomize poly) coefs
    subPolynome <- translator pow0 rest
    let finalList = case subPolynome of
                         Nothing -> result
                         Just p -> (CoeffInt 0, p) : result
    return . Just $ Polynome var finalList

translator pow0 [] = return $ PolyRest <$> evalCoeff pow0

-- | Try to transform a formula in polynome.
polynomize :: FormulaPrim -> Maybe Polynome
polynomize wholeFormula@(BinOp OpMul _) = polynomize (BinOp OpAdd [wholeFormula])
-- HMmm?
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

extractFirstTerm (BinOp OpPow [Variable v, order])
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
polyMap f (Polynome s lst) = Polynome s $ map (\(c,p) -> (c, polyMap f p)) lst
polyMap f rest@(PolyRest _) = snd $ f (CoeffInt 0, rest)

-- | Transform a scalar formula component to
-- a polynome coefficient. If formula is not
-- a scalar, error is called.
scalarToCoeff :: FormulaPrim -> PolyCoeff
scalarToCoeff (UnOp OpNegate f) = negate $ scalarToCoeff f
scalarToCoeff (CFloat f) = CoeffFloat f
scalarToCoeff (CInteger i) = CoeffInt i
scalarToCoeff (BinOp OpDiv [CInteger a, CInteger b]) = CoeffRatio $ a % b
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
lockStep  _ xs [] = xs
lockStep  _ [] ys = ys
lockStep op whole1@((c1, def1):xs) whole2@((c2, def2):ys)
    | c1 `inf` c2 = 
        (c1, def1 `op` PolyRest (CoeffInt 0)) : lockStep op xs whole2
    | c1  ==   c2 = 
        (c1, def1 `op` def2) : lockStep op xs ys
    | otherwise   =
        (c2, PolyRest (CoeffInt 0) `op` def2) : lockStep op whole1 ys

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
-- FUCKING WRONG for '-'
polySimpleOp op left@(PolyRest _) right@(Polynome _ _) =
    polySimpleOp (flip op) right left
polySimpleOp op (Polynome v1 as@((coeff, def):xs)) right@(PolyRest c1)
    | isCoeffNull coeff = case def of
        PolyRest a -> Polynome v1 $ (CoeffInt 0, PolyRest $ coeffOp op a c1) : map (coeffPropagator op) xs
        _          -> Polynome v1 $ (coeff,polySimpleOp op def right) : map (coeffPropagator op) xs
    | otherwise = 
        Polynome v1 $ (CoeffInt 0, PolyRest $ coeffOp op (CoeffInt 0) c1):as

polySimpleOp op (Polynome v1 as@((c, d1):rest)) left@(Polynome v2 bs)
    | v1 > v2 = polySimpleOp (flip op) (Polynome v2 bs) (Polynome v1 as)
    | v1 == v2 = Polynome v1 $ lockStep op as bs
    | isCoeffNull c = case d1 of 
          PolyRest a   -> polyMap executor left
                where executor (c', PolyRest n) = (c', PolyRest $ coeffOp op a n)
                      executor a' = a'
          Polynome _ _ -> Polynome v1 $ (CoeffInt 0, polySimpleOp op d1 left) : map (coeffPropagator op) rest
    | otherwise = Polynome v1 $ (CoeffInt 0, PolyRest (CoeffInt 0) `op` left) : map (coeffPropagator op) as


-- | Meh!
divisable :: Polynome -> Polynome -> Bool
divisable (PolyRest _) (Polynome _ _) = False
divisable (Polynome _ _) (PolyRest _) = True
divisable (Polynome rootVar lst) right@(Polynome divVar [lst'@(coef, sub)])
    | rootVar < divVar = False
    | rootVar > divVar = 
            let subDivide (_,(PolyRest _)) = False
                subDivide (_,p@(Polynome _ _)) = divisable p right
            in all subDivide lst
    | otherwise = null smaller && all (flip divisable sub) (map snd bigger)
        where greater (coeff1,_) = coeff1 >= coef
              (bigger, smaller) = partition greater lst
divisable _ _ = error "Polynome.divisable - meh"

-- | Multiply two polynomials between them using the brute force
-- way, algorithm in O(n²)
polyMul :: Polynome -> Polynome -> Polynome
polyMul p@(Polynome _ _) (PolyRest c) = polyCoeffMap (\a -> a * c) p
polyMul (PolyRest c) p@(Polynome _ _) = polyCoeffMap (\a -> c * a) p
polyMul (PolyRest c) (PolyRest c2) = PolyRest $ coeffOp (*) c c2
polyMul p1@(Polynome v1 _) p2@(Polynome v2 _) | v1 > v2 = polyMul p2 p1
polyMul (Polynome v1 coefs1) p2@(Polynome v2 coefs2)
    | v1 /= v2 {- v1 < v2 by previous line -} =
        Polynome v1 $ map (\(order, c) -> (order, polyMul c p2)) coefs1
    | otherwise {- v1 == v2 -} =
        Polynome v1
      {-. map (\lst@((o,_):_) -> (o, foldr1 (+) $ map snd lst))-}
      . map (\lst@((o,_):_) -> (o, sum $ map snd lst))
      . groupBy (\(o1,_) (o2,_) -> o1 == o2) -- Regroup same order together
      $ sortBy (\(c1,_) (c2,_) -> compare c1 c2)
      [ (degree1 + degree2, c1 * c2) | (degree1, c1) <- coefs1, (degree2, c2) <- coefs2]


-- f / g = q + r / g
{-polyQuotRem :: Polynome -> Polynome-}
            {--> (Maybe Polynome, Maybe Polynome) -- ^ Quotient and rest-}
{-polyQuotRem (Polynome v1 lst) (PolyRest coeff)-}
{-polyQuotRem (PolyRest coeff) (Polynome v1 lst)-}
{-polyQuotRem (Polynome v1 lst) (Polynome v2 lst')-}
    {-| v1 == v2 = (Nothing, Nothing)-}
        {-where diver :: [(PolyCoeff, Polynome)]-}
                    {--> [(PolyCoeff, Polynome)]-}
                    {--> [(PolyCoeff, Polynome)]-}
                    {--> [(PolyCoeff, Polynome)]-}
                    {--> ([(PolyCoeff, Polynome)], [(PolyCoeff, Polynome)])-}
              {-diver f quotient rest [] = (quotient, rest)-}
              {-diver f quotient rest (x:xs)-}
                    {-| divisable x = ([], [])-}
                    {-| otherwise = diver (tail f) quotient (head f : rest)-}

              {-divisable :: [(PolyCoeff, Polynome)] -> (PolyCoeff, Polynome) -> Bool-}
              {-divisable _ (coeff, PolyRest _) | isCoeffNull coeff = True-}
                                              {-| otherwise = False-}
              {-divisable (Polynome _ lst) lst'@(coef, sub) =-}
                  {-null smaller && all recurseDivide (map snd bigger)-}
                      {-where (bigger, smaller) = partition (\(coeff1,_) -> coeff1 >= coef) lst-}
                            {-recurseDivide = divisable (Polynome "" p) (CoeffInt, sub)-}

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

