module EqManips.Algorithm.MetaEval ( metaEval ) where

import EqManips.Algorithm.Utils
import EqManips.Algorithm.Expand
import EqManips.Types
import EqManips.EvaluationContext

-- | The only meta evaluation avaible
metaEval :: (Formula -> EqContext Formula) -> MetaOperation -> Formula
         -> EqContext Formula
metaEval evaluator Force f = evaluator f
metaEval _ Hold f = return f
metaEval evaluator Listify f = return . listifyBinOp =<< evaluator f
metaEval evaluator Treefy f = return . treeIfyBinOp =<< evaluator f
metaEval _ Expand f = return $ expand f

