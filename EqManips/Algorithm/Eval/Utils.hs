module EqManips.Algorithm.Eval.Utils where

import Control.Applicative

import EqManips.Types
import EqManips.EvaluationContext
import EqManips.Algorithm.Eval.Types
import EqManips.Algorithm.Utils
import EqManips.Propreties

import Data.List( sort )

left :: (Monad m) => a -> m (Either a b)
left = return . Left

right :: (Monad m) => b -> m (Either a b)
right = return . Right

-- | Used to transform a binop to a scalar if size
-- is small
binOp :: BinOperator -> [FormulaPrim] -> FormulaPrim
binOp _ [x] = x
binOp op lst = BinOp op lst

-- | Evaluate a binary operator
binEval :: BinOperator -> EvalOp -> EvalOp -> [FormulaPrim] -> EqContext FormulaPrim
binEval op f inv formulaList 
    | op `hasProp` Associativ && op `hasProp` Commutativ =
#ifdef _DEBUG
        addTrace ("Sorting => ", treeIfyFormula . Formula $ BinOp op formulaList) >>
#endif
        binOp op <$> biAssocM f inv (sort formulaList)

    | otherwise =
#ifdef _DEBUG
        addTrace ("Basic Eval=>", treeIfyFormula . Formula $ BinOp op formulaList) >>
#endif
        binOp op <$> biAssocM f inv formulaList

