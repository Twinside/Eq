module EqManips.Algorithm.MetaEval ( metaEval ) where

import EqManips.Algorithm.Utils
import EqManips.Algorithm.Expand
import EqManips.Algorithm.Cleanup
import EqManips.Types
import EqManips.EvaluationContext

import qualified EqManips.ErrorMessages as Err

-- | The only meta evaluation avaible
metaEval :: (Formula -> EqContext Formula) -> MetaOperation -> Formula
         -> EqContext Formula
metaEval evaluator Force f = evaluator f
metaEval evaluator Cleanup f = return . cleanup =<< evaluator f
metaEval _ Hold f = return f
metaEval evaluator Listify f = return . listifyBinOp =<< evaluator f
metaEval evaluator Treefy f = return . treeIfyBinOp =<< evaluator f
metaEval _ Expand f = return $ expand f
metaEval evaluator Sort f = return . sortFormula =<< evaluator f
metaEval evaluator LambdaBuild (Lambda [([arg], body)]) = do
    arg' <- evaluator arg 
    body' <- evaluator body
    return $ Lambda [([arg'], body')]
metaEval _ LambdaBuild _ = eqFail (Block 1 1 1) Err.wrong_lambda_format 
