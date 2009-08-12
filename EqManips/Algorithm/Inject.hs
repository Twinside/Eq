module EqManips.Algorithm.Inject( inject ) where

import Data.Maybe( fromMaybe )
import EqManips.Types
import EqManips.FormulaIterator
import EqManips.EvaluationContext

-- | Replace all variables that get a definition by
-- their definition if there is one. Otherwise let
-- the variable like that.
inject :: Formula -> EqContext Formula
inject = depthFormulaTraversal scopePreserver injectIntern

-- | This function perform a sort of alpha
-- renaming on subScope, it's called when arriving
-- on a node, to prevent wrong replacements.
scopePreserver :: Formula -> EqContext ()
scopePreserver f = keepSafe $ reBoundVar f
    where keepSafe Nothing = return ()
          keepSafe (Just v) = do
              pushContext
              delSymbol v

injectIntern :: Formula -> EqContext Formula
injectIntern f@(Variable v) =
    symbolLookup v >>= return . fromMaybe f

injectIntern f = scope $ reBoundVar f
    where scope Nothing = return f
          scope _ = popContext >> return f
                 
-- | Tell if a node change the scope.
-- The pattern is explicitely exaustive to be sure
-- to get the compiler shout if a change is made.
reBoundVar :: Formula -> Maybe String
reBoundVar (Product (BinOp OpEq (Variable v:_)) _ _) = Just v
reBoundVar (Sum (BinOp OpEq (Variable v: _)) _ _) = Just v

reBoundVar (Variable _) = Nothing
reBoundVar (NumEntity _) = Nothing
reBoundVar (CInteger _) = Nothing
reBoundVar (CFloat _) = Nothing
reBoundVar (App _ _) = Nothing
reBoundVar (Derivate _ _) = Nothing
reBoundVar (Integrate _ _ _ _) = Nothing
reBoundVar (UnOp _ _) = Nothing
reBoundVar (BinOp _ _) = Nothing
reBoundVar (Matrix _ _ _) = Nothing
reBoundVar (Block _ _ _) = Nothing
reBoundVar (Product _ _ _) = Nothing
reBoundVar (Sum _ _ _) = Nothing
-- Nothing preserved during evaluation normaly.
reBoundVar (Lambda _) = Nothing

