{-# LANGUAGE Rank2Types #-}
module EqManips.Algorithm.Eval( reduce
                              , exactReduce 
                              , evalGlobalLossyStatement 
                              , evalGlobalLosslessStatement 
                              ) where

import EqManips.Types

import EqManips.Algorithm.Cleanup

import EqManips.Algorithm.Eval.GenericEval
import EqManips.Algorithm.Eval.GlobalStatement
import EqManips.Algorithm.Eval.Floating
import EqManips.Algorithm.Eval.Polynomial
import EqManips.Algorithm.Eval.Ratio
import EqManips.Algorithm.Eval.Complex
import EqManips.Algorithm.Eval.Types

evalGlobalLossyStatement, evalGlobalLosslessStatement :: FormulaEvaluator
evalGlobalLossyStatement = evalGlobalStatement reduce'
evalGlobalLosslessStatement = evalGlobalStatement exactReduce'

-- | Main function to evaluate another function
reduce :: FormulaEvaluator
reduce = taggedEvaluator reduce'

-- | Main function to evaluate raw formula
reduce' :: EvalFun
reduce' f = (eval reduce' $ cleaner f)
        >>= polyEvalRules reduce' . cleaner
        >>= floatEvalRules
    where cleaner = unTagFormula . cleanupRules . Formula

-- | Only perform non-lossy transformations
exactReduce :: FormulaEvaluator
exactReduce = taggedEvaluator exactReduce'

-- | same as exactReduce, but perform on raw formula.
exactReduce' :: EvalFun
exactReduce' f = (eval exactReduce' $ cleaner f)
           >>= polyEvalRules exactReduce'
    where cleaner = unTagFormula . cleanupRules . Formula

