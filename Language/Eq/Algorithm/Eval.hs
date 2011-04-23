{-# LANGUAGE Rank2Types #-}
module Language.Eq.Algorithm.Eval( reduce
                              , exactReduce 
                              , evalGlobalLossyStatement 
                              , evalGlobalLosslessStatement 
                              ) where

import Language.Eq.Types

import Language.Eq.Algorithm.Cleanup

import Language.Eq.Algorithm.Eval.GenericEval
import Language.Eq.Algorithm.Eval.GlobalStatement
import Language.Eq.Algorithm.Eval.Floating
import Language.Eq.Algorithm.Eval.Polynomial
import Language.Eq.Algorithm.Eval.Ratio
import Language.Eq.Algorithm.Eval.Complex
import Language.Eq.Algorithm.Eval.Types

import Language.Eq.Algorithm.Simplify

evalGlobalLossyStatement, evalGlobalLosslessStatement :: FormulaEvaluator
evalGlobalLossyStatement = evalGlobalStatement reduce'
evalGlobalLosslessStatement = evalGlobalStatement exactReduce'

-- | Main function to evaluate another function
reduce :: FormulaEvaluator
reduce = taggedEvaluator reduce'

-- | Main function to evaluate raw formula
reduce' :: EvalFun
reduce' f = eval reduce' (cleaner f)
        >>= ratioEvalRules
        >>= complexEvalRules reduce'
        >>= polyEvalRules reduce' . cleaner
        >>= floatEvalRules . cleaner
        >>= simplifyFormula reduce'
        >>= return . cleaner
    where cleaner = unTagFormula . cleanupRules . Formula

-- | Only perform non-lossy transformations
exactReduce :: FormulaEvaluator
exactReduce = taggedEvaluator exactReduce'

-- | same as exactReduce, but perform on raw formula.
exactReduce' :: EvalFun
exactReduce' f = eval exactReduce' (cleaner f)
             >>= ratioEvalRules
             >>= complexEvalRules exactReduce'
             >>= polyEvalRules exactReduce' . cleaner
             >>= simplifyFormula reduce'
    where cleaner = unTagFormula . cleanupRules . Formula

