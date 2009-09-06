module EqManips.Algorithm.Expand ( expand ) where

import EqManips.Types
import EqManips.Algorithm.Utils
import EqManips.FormulaIterator
import EqManips.Propreties

-- | Algorithm to call to perform a global formula
-- expension
expand :: Formula -> Formula
expand f = listifyFormula
         $ depthFirstFormula `asAMonad` expander 
         $ treeIfyFormula f

-- | Filter used to perform formula expansion.
expander :: Formula -> Formula
expander (BinOp op [a,b])
    | op `hasProp` Distributiv = 
        distributeLeft op (BinOp op) a b
expander f = f

-- | The role of this function is to search all pseudo-end
-- nodes in the right formula and then launch another matching
-- which will really create new nodes.
distributeLeft :: BinOperator            -- ^ Priority of distributiv operator
               -> ([Formula] -> Formula) -- ^ Combine two sub-formulas
               -> Formula -> Formula -> Formula
distributeLeft op combine formula (BinOp op' [a,b]) 
    | not $ op `canDistributeOver` op'
    = BinOp op' [digg a, digg b]
        where digg = distributeLeft op combine formula

distributeLeft _iniPrio combine formula with =
    distributeRight combine formula with

-- | Really apply the distributivity.
distributeRight :: ([Formula] -> Formula)
                -> Formula -> Formula -> Formula
distributeRight combine (BinOp op [a,b]) sub
    | not $ op `hasProp` Distributiv = BinOp op [digg a, digg b]
        where digg tree = distributeRight combine tree sub
distributeRight combine op sub = combine [op, sub]

