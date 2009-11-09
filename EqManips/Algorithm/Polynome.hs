{-# LANGUAGE ScopedTypeVariables #-}
module EqManips.Algorithm.Polynome( convertToPolynome ) where

import Control.Applicative( (<$>), (<*>) )
import Control.Monad( join )
import Data.Either( partitionEithers )
import Data.List( groupBy, foldl' )
import Data.Ratio
import EqManips.Types
import EqManips.Algorithm.Utils
import EqManips.FormulaIterator
import qualified EqManips.ErrorMessages as Err

import EqManips.Renderer.Ascii

-- | Given a formula, it'll try to convert it to a polynome.
-- Formula should be expanded and in list form to get this
-- function to work (nested shit shouldn't work)
{-convertToPolynome :: Formula ListForm -> Maybe Polynome-}
convertToPolynome (Formula f) = (prepareFormula f, polynomize $ prepareFormula f)

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

          sorter _ (BinOp OpPow (Variable v2:_)) = GT
          sorter (BinOp OpPow (Variable v1:_)) _ = LT

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
          atomicResign a = Nothing
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
   where countInversion whole@(UnOp OpNegate _) acc =
             if odd . fst $ getUnsignedRoot 0 whole
                then not acc
                else acc
         countInversion _ acc = acc

         getUnsignedRoot n (UnOp OpNegate something) = getUnsignedRoot (n+1) something
         getUnsignedRoot n (something) = (n, something)

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
                  nameOfGroup [] = error "EqManips.Algorithm.Polynome.polynomize.packCoefs meh"

                  grouper :: [(String,FormulaPrim,FormulaPrim)] -> (String, [(FormulaPrim,FormulaPrim)])
                  grouper lst' = (nameOfGroup lst'
                                 , [(coef group, BinOp OpAdd $ defs group) | group <- coeffGroup lst'])
                  defs = map (\(_,_,def) -> def)
                  coef ((_,c1,_):_) = c1
                  coef [] = error "EqManips.Algorithm.Polynome.polynomize.packCoefs.coef meh"

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

-- | polynome
{-polyMap :: ((FormulaPrim, Polynome) -> (FormulaPrim, Polynome)) -> Polynome -> Polynome-}
{-polyMap f (Polynome s lst) =-}

{-polyOp :: Polynome-}
{-polyOp op-}


{-instance Num Polynome where-}
    {-(Polynome v1 as) + (Polynome v2 bs)-}
        {-| v1 == v2 =-}

