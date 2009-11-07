module EqManips.Algorithm.Polynome( convertToPolynome ) where

import Control.Applicative
import Data.List( groupBy, foldl' )
import Data.Maybe( catMaybes )
import EqManips.Types
import EqManips.Algorithm.Utils
import EqManips.FormulaIterator
import qualified EqManips.ErrorMessages as Err

import EqManips.Renderer.Ascii

convertToPolynome :: Formula ListForm -> IO (Maybe Polynome)
convertToPolynome (Formula f) = do
    let f' = prepareFormula f
    putStr "\n=================================================\n"
    putStr $ show f
    putStr "\n=================================================\n\n"
    putStr $ show f'
    putStr "\n=================================================\n\n"
    putStr $ formatFormula $ treeIfyFormula (Formula f')
    putStr "\n=================================================\n\n"
    return $ polynomize f'


prepareFormula :: FormulaPrim -> FormulaPrim
prepareFormula = 
                 {-unTagFormula-}
                 polySort
               {-. invSortFormula-}
               {-. Formula-}
               . formulaFlatter
    where polySort = depthFormulaPrimTraversal `asAMonad` (sortBinOp sorter)
          sorter (Variable v1) (Variable v2) = invert $ compare v1 v2
          sorter (BinOp OpPow [Variable v1, _p1])
                 (BinOp OpPow [Variable v2, _p2]) = compare v1 v2

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

-- | Helper to write minimal binop node.
opify :: BinOperator -> [FormulaPrim] -> FormulaPrim
opify _ [] = error $ Err.empty_binop "Polynome.opify"
opify _ [x] = x
opify op alist = BinOp op alist

-- | /!\ INCOMPLETE !!!!!
-- TO FIX LATER
evalCoeff :: FormulaPrim -> Maybe Integer
evalCoeff (CInteger i) = Just i
evalCoeff _ = Nothing

-- | TODO: add a real comment
translator :: [[(String, FormulaPrim, FormulaPrim)]] -> Maybe Polynome
translator [first@((var,_,_):_)] = do 
        result <- mapM (\(_,rank,poly) -> (,) <$> evalCoeff rank <*> polynomize poly) first
        return $ Polynome var result

translator (first@((var,_,_):_):rest) = do
    subPolynome <- translator rest
    result <- mapM (\(_,rank,poly) -> (,) <$> evalCoeff rank <*> polynomize poly) first
    let finalList = (0, subPolynome) : result
    return $ Polynome var finalList

translator _ = Nothing

-- | TODO: add a real comment
polynomize :: FormulaPrim -> Maybe Polynome
polynomize (BinOp OpAdd lst) =
    translator
  . groupBy (\(var,_,_) (var',_,_) -> var == var')
  . catMaybes
  $ map (extractFirstTerm . listFlatter) lst

polynomize _ = Nothing


-- | TODO: add a real comment
extractFirstTerm :: FormulaPrim -> Maybe (String, FormulaPrim, FormulaPrim)
extractFirstTerm (BinOp OpMul lst) = varCoef lst
    where varCoef ((BinOp OpPow [(Variable v), f]):xs)
                | isFormulaConstant f = Just (v, f, multify xs)
          varCoef ((Variable v):xs) = Just (v, CInteger 1, multify xs)
          varCoef _ = Nothing
        
          multify = opify OpMul
extractFirstTerm _ = Nothing
