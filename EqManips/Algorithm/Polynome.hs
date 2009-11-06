module EqManips.Algorithm.Polynome  where

import Control.Applicative
import Data.List( groupBy )
import Data.Maybe( catMaybes )
import EqManips.Types
import EqManips.Algorithm.Utils
import EqManips.FormulaIterator
import qualified EqManips.ErrorMessages as Err

prepareFormula :: FormulaPrim -> FormulaPrim
prepareFormula = unTagFormula . invSortFormula . Formula . formulaFlatter

-- | We assume that the formula as been previously sorted
resign :: FormulaPrim -> FormulaPrim
resign (BinOp OpMul (CInteger n: xs)) = BinOp OpMul $ CInteger (-n) : xs
resign (BinOp OpMul (CFloat n: xs)) = BinOp OpMul $ CFloat (-n) : xs
resign (BinOp OpMul (a:xs)) | isFormulaInteger a = BinOp OpMul $ CInteger (-1):a:xs
resign a = BinOp OpMul [CInteger (-1), a]

formulaFlatter :: FormulaPrim -> FormulaPrim
formulaFlatter = depthFormulaPrimTraversal `asAMonad` listFlatter

-- | Given a formula in LIST form, provide a version
-- with only Pluses.
listFlatter :: FormulaPrim -> FormulaPrim
listFlatter (BinOp OpAdd lst) = BinOp OpAdd $ foldr flatter [] lst
    where flatter (BinOp OpSub (x:xs)) acc = x : map resign xs ++ acc
          flatter x acc = x:acc
listFlatter a = a

-- | Helper to write minimal binop node.
opify :: BinOperator -> [FormulaPrim] -> FormulaPrim
opify _ [] = error $ Err.empty_binop "Polynome.opify"
opify _ [x] = x
opify op alist = BinOp op alist

evalCoeff :: FormulaPrim -> Maybe Integer
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
