module EqManips.Algorithm.Eval.Meta ( metaEval ) where

import EqManips.Algorithm.Utils
import EqManips.Algorithm.Expand
import EqManips.Algorithm.Cleanup
import EqManips.Types
import EqManips.EvaluationContext

import qualified EqManips.ErrorMessages as Err

-- | The only meta evaluation avaible
metaEval :: (Formula ListForm -> EqContext (Formula ListForm))
         -> MetaOperation
         -> Formula ListForm
         -> EqContext (Formula ListForm)
metaEval evaluator Force f = evaluator f
metaEval evaluator Cleanup f = return . cleanup =<< evaluator f
metaEval _ Hold f = return f
metaEval _ Expand f = return . listifyFormula . expand . treeIfyFormula $ f

metaEval evaluator Sort f = return . sortFormula =<< evaluator f
metaEval evaluator LambdaBuild (Formula (Lambda _ [([arg], body)])) = do
    arg' <- evaluator . Formula $ arg 
    body' <- evaluator . Formula $ body
    return . Formula $ lambda [([unTagFormula arg'], unTagFormula body')]
metaEval _ LambdaBuild _ = eqFail (Formula $ Block 1 1 1) Err.wrong_lambda_format 

