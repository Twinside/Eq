module Language.Eq.Algorithm.Eval.Utils( left
                                    , right
                                    , binOpReducer
                                    , binEval
                                    ) where

import Control.Applicative
import Data.List( sort, foldl' )

import Language.Eq.Types
import Language.Eq.EvaluationContext
import Language.Eq.Algorithm.Eval.Types
import Language.Eq.Algorithm.Utils
import Language.Eq.Propreties

left :: (Monad m) => a -> m (Either a b)
left = return . Left

right :: (Monad m) => b -> m (Either a b)
right = return . Right

-- | Used to transform a binop to a scalar if size
-- is small
binOpReducer :: BinOperator -> [FormulaPrim] -> FormulaPrim
binOpReducer _ [x] = x
binOpReducer op lst = binOp op lst

-- | Assuming children in list form, parse the list to 
-- keep the general listform.
binListRepacker :: BinOperator -> [FormulaPrim] -> FormulaPrim
binListRepacker op lst = binOpReducer op
                       $ foldl' emergeSubOp id lst []
    where emergeSubOp acc (BinOp _ op2 subLst)
                | op == op2 = acc . (subLst ++)
          emergeSubOp acc sub = acc . (sub:)

-- | Evaluate a binary operator
-- Right associative operators are called with arguments reversed!
binEval :: BinOperator -> EvalOp -> EvalOp -> [FormulaPrim] -> EqContext FormulaPrim
binEval op f inv formulaList 
    | op `hasProp` Associativ && op `hasProp` Commutativ =
#ifdef _DEBUG
        addTrace ("Sorting => ", treeIfyFormula . Formula $ binOp op formulaList) >>
#endif
        binListRepacker op <$> biAssocM f inv (sort formulaList)

    | op `obtainProp` AssocSide == OpAssocRight =
#ifdef _DEBUG
        addTrace ("Basic Right Eval=>", treeIfyFormula . Formula $ binOp op formulaList) >>
#endif
        binListRepacker op . reverse <$> (biAssocM f inv $ reverse formulaList)

    | otherwise =
#ifdef _DEBUG
        addTrace ("Basic Eval=>", treeIfyFormula . Formula $ binOp op formulaList) >>
#endif
        binListRepacker op <$> biAssocM f inv formulaList

