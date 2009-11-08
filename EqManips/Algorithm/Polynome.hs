{-# LANGUAGE ScopedTypeVariables #-}
module EqManips.Algorithm.Polynome( convertToPolynome ) where

import Control.Applicative
import Data.List( groupBy, foldl' )
import Data.Either( partitionEithers )
import EqManips.Types
import EqManips.Algorithm.Utils
import EqManips.FormulaIterator
import qualified EqManips.ErrorMessages as Err

import EqManips.Renderer.Ascii

convertToPolynome :: Formula ListForm -> IO (Maybe Polynome)
convertToPolynome (Formula f) = do
    let f' = prepareFormula f
    putStr "\n=================================================\n"
    putStrLn $ show f'
    putStr "=================================================\n\n"
    putStr "\n=================================================\n"
    putStrLn $ formatFormula $ treeIfyFormula (Formula f')
    putStr "=================================================\n\n"
    return $ polynomize f'

-- Preparation de la formule {{{
-- | Flatten the formula, remove all the OpSub and replace them
-- by OpAdd. Also bring lowest variables to the front, regardless of
-- their order.
prepareFormula :: FormulaPrim -> FormulaPrim
prepareFormula = polySort . formulaFlatter
    where polySort = depthFormulaPrimTraversal `asAMonad` (sortBinOp sorter)
          -- Special sort which bring x in front, followed by others. Lexical
          -- order first.

          -- Rules to fine-sort '*' elements
          sorter (Variable v1) (Variable v2) = invert $ compare v1 v2
          sorter (BinOp OpPow [Variable v1, _p1])
                 (BinOp OpPow [Variable v2, _p2]) = compare v1 v2

          sorter (Variable v1)
                 (BinOp OpPow (Variable v2:_)) = compare v1 v2

          -- Rules to fine sort the '+' elements
          sorter (BinOp OpMul (BinOp OpPow (Variable v1:_):_))
                 (BinOp OpMul (BinOp OpPow (Variable v2:_):_)) = compare v1 v2

          sorter (BinOp OpMul (Variable v1:_))
                 (BinOp OpMul (BinOp OpPow (Variable v2:_):_)) = compare v1 v2

          sorter (BinOp OpMul (BinOp OpPow (Variable v1:_):_))
                 (BinOp OpMul (Variable v2:_)) = compare v1 v2

          sorter (BinOp OpPow a) (BinOp OpPow b) =
                case compare (length a) (length b) of
                     LT -> LT
                     GT -> GT
                     EQ -> foldl' (\acc (a', b') -> if acc == EQ
                                                        then acc
                                                        else compare a' b') EQ $ zip a b
          sorter a b = invert $ compare a b

          invert LT = GT
          invert EQ = EQ
          invert GT = LT

-- | Called when we found an OpSub operator within the
-- formula.
-- We assume that the formula as been previously sorted
resign :: FormulaPrim -> [FormulaPrim] -> [FormulaPrim]
resign (BinOp OpMul (CInteger n: xs)) acc = BinOp OpMul (CInteger (-n) : xs) : acc
resign (BinOp OpMul (CFloat n: xs)) acc = BinOp OpMul (CFloat (-n) : xs) : acc
resign (BinOp OpMul (a:xs)) acc | isFormulaInteger a = BinOp OpMul (CInteger (-1):a:xs) : acc
resign (BinOp OpAdd lst) acc = foldr resign acc lst
resign a acc = BinOp OpMul [CInteger (-1), a] : acc

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
listFlatter a = a
-- }}}

-- | Helper to write minimal binop node.
opify :: BinOperator -> [FormulaPrim] -> FormulaPrim
opify _ [] = error $ Err.empty_binop "Polynome.opify"
opify _ [x] = x
opify op alist = BinOp op alist

-- | /!\ INCOMPLETE !!!!!
-- TO FIX LATER
evalCoeff :: FormulaPrim -> Maybe FormulaPrim
evalCoeff (CInteger i) = Just (CInteger i)
evalCoeff _ = Nothing

-- | TODO: add a real comment
translator :: [FormulaPrim]                          -- Unnammed rest (var ^ 0)
           -> [(String, [(FormulaPrim, FormulaPrim)])] -- Named things x ^ n or y ^ n, n > 0
           -> Maybe Polynome
translator [] [(var, coefs)] = do 
        result <- mapM (\(rank,poly) -> (,) <$> evalCoeff rank <*> polynomize poly) coefs
        return $ Polynome var result

translator pow0 [(var, coefs)] = do 
        result <- mapM (\(rank,poly) -> (,) <$> evalCoeff rank <*> polynomize poly) coefs
        return . Polynome var $ (0, PolyCoeffFo $ BinOp OpAdd pow0):result

translator pow0 ((var,coefs):rest) = do
    result <- mapM (\(rank,poly) -> (,) rank <$> polynomize poly) coefs
    let subPolynome = translator pow0 rest
        finalList = case subPolynome of
                         Nothing -> result
                         Just p -> (0, p) : result
    return $ Polynome var finalList

translator [pow0] [] = Just $ PolyCoeffFo pow0
translator pow0 [] = Just $ PolyCoeffFo $ BinOp OpAdd pow0
{-translator ba [] = error $ "EqManips.Algorithm.Polynome.translator " ++ "Empty bidule :-/"-}

-- | Try to transform a formula in polynome. Still not
-- finished.
polynomize :: FormulaPrim -> Maybe Polynome
polynomize (BinOp OpAdd lst) = translator pow0 
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

polynomize a = Just $ PolyCoeffFo a

-- | Function in charge of extracting variable name (if any), and
-- return the coeff function.
extractFirstTerm :: FormulaPrim
                 -> Either (String, FormulaPrim, FormulaPrim) FormulaPrim
extractFirstTerm fullFormula@(BinOp OpMul lst) = varCoef lst
    where varCoef ((BinOp OpPow [(Variable v), f]):xs)
                | isFormulaConstant f = Left (v, f, multify xs)
          varCoef ((Variable v):xs) = Left (v, CInteger 1, multify xs)
          varCoef _ = Right fullFormula
        
          multify = opify OpMul
extractFirstTerm a = Right a

