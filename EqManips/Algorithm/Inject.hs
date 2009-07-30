module EqManips.Algorithm.Inject( inject ) where

import Data.Maybe( fromMaybe )
import EqManips.Types
import EqManips.FormulaIterator
import EqManips.EvaluationContext( EqContext, symbolLookup )

-- | Replace all variables that get a definition by
-- their definition if there is one. Otherwise let
-- the variable like that.
inject :: Formula -> EqContext Formula
inject = formulaIterate injectIntern

injectIntern :: Formula -> EqContext Formula
injectIntern f@(Variable v) = do
    foundVar <- symbolLookup v
    return $ fromMaybe f foundVar

injectIntern f | isFormulaLeaf f = return f
               | otherwise = formulaIterate injectIntern f
                 
