module EqManips.Algorithm.Expand ( expand ) where

import EqManips.Types
import EqManips.Algorithm.Utils
import EqManips.FormulaIterator
import EqManips.Propreties

-- | Algorithm to call to perform a global formula
-- expension
expand :: Formula TreeForm -> Formula TreeForm
expand (Formula f) = Formula
                   $ depthFormulaPrimTraversal `asAMonad` expander 
                   $ f

-- | Filter used to perform formula expansion.
expander :: FormulaPrim -> FormulaPrim
expander (BinOp _ op [a,b])
    | op `hasProp` Distributiv = 
        distributeLeft op (binOp op) a b
expander f = f

-- | The role of this function is to search all pseudo-end
-- nodes in the right formula and then launch another matching
-- which will really create new nodes.
distributeLeft :: BinOperator            -- ^ Priority of distributiv operator
               -> ([FormulaPrim] -> FormulaPrim) -- ^ Combine two sub-formulas
               -> FormulaPrim
               -> FormulaPrim
               -> FormulaPrim
distributeLeft op combine formula (BinOp _ op' [a,b]) 
    | not $ op `canDistributeOver` op'
    = binOp op' [digg a, digg b]
        where digg = distributeLeft op combine formula

distributeLeft _iniPrio combine formula with =
    distributeRight combine formula with

-- | Really apply the distributivity.
distributeRight :: ([FormulaPrim] -> FormulaPrim)
                -> FormulaPrim -> FormulaPrim -> FormulaPrim
distributeRight combine (BinOp _ op [a,b]) sub
    | not $ op `hasProp` Distributiv = binOp op [digg a, digg b]
        where digg tree = distributeRight combine tree sub
distributeRight combine op sub = combine [op, sub]

