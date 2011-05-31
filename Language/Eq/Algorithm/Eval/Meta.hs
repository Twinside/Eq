module Language.Eq.Algorithm.Eval.Meta ( metaEval
                                    , metaFilter
                                    ) where

import Control.Applicative
import Data.List( sort )

import Language.Eq.Algorithm.Utils
import Language.Eq.Algorithm.Expand
import Language.Eq.Algorithm.Cleanup
import Language.Eq.Algorithm.Eval.Types
import Language.Eq.Types
import Language.Eq.EvaluationContext
import Language.Eq.FormulaIterator

import qualified Language.Eq.ErrorMessages as Err

-- | The only meta evaluation avaible
metaEval :: (Formula ListForm -> EqContext (Formula ListForm))
         -> MetaOperation
         -> Formula ListForm
         -> EqContext (Formula ListForm)
metaEval evaluator Force f = evaluator f
metaEval evaluator Cleanup f = return . cleanup =<< evaluator f
metaEval _ Hold f = return f
metaEval _ Expand f = return . listifyFormula . expand . treeIfyFormula $ f

metaEval evaluator Sort (Formula (List _ lst)) =
    Formula . list . sort <$> mapM unclap lst
        where unclap formu = unTagFormula <$> evaluator (Formula formu)
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

