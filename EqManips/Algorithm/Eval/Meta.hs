module EqManips.Algorithm.Eval.Meta ( metaEval
                                    , metaFilter
                                    ) where

import Control.Applicative

import EqManips.Algorithm.Utils
import EqManips.Algorithm.Expand
import EqManips.Algorithm.Cleanup
import EqManips.Algorithm.Eval.Types
import EqManips.Types
import EqManips.EvaluationContext
import EqManips.FormulaIterator

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
    arg' <- metaFilter (\a -> unTagFormula <$> (evaluator $ Formula a)) arg
    body' <- metaFilter (\a -> unTagFormula <$> (evaluator $ Formula a)) body
    return . Formula $ lambda [([arg'], body')]
metaEval _ LambdaBuild _ = eqFail (Formula $ Block 1 1 1) Err.wrong_lambda_format 


-- | Run across the formula to find meta evaluation and then
-- evaluate it. Used to level the use of Force/Hold & everyting.
metaFilter :: EvalFun -> FormulaPrim -> EqContext FormulaPrim
metaFilter evaluator formu = topDownScanning metaCatch formu
    where metaCatch (Meta _ op f) = Just . unTagFormula
                                 <$> (metaEval eval' op $ Formula f)
          metaCatch _ = pure Nothing

          eval' a = Formula <$> (evaluator $ unTagFormula a)

