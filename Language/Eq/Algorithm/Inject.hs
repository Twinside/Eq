{-# LANGUAGE ScopedTypeVariables #-}
module Language.Eq.Algorithm.Inject( inject ) where

import Control.Applicative
import Language.Eq.Types
import Language.Eq.FormulaIterator
import Language.Eq.EvaluationContext
import Language.Eq.Algorithm.Utils
import Language.Eq.Algorithm.Eval.Polynomial

-- | Replace all variables that get a definition by
-- their definition if there is one. Otherwise let
-- the variable like that.
inject :: Formula ListForm -> EqContext (Formula ListForm)
inject (Formula f) = do
#ifdef _DEBUG
    addTrace ("Injection:", Formula $ f)
#endif
    Formula <$> depthPrimTraversal scopePreserver injectIntern f

-- | This function perform a sort of alpha
-- renaming on subScope, it's called when arriving
-- on a node, to prevent wrong replacements.
scopePreserver :: FormulaPrim -> EqContext ()
scopePreserver f = keepSafe $ reBoundVar f
    where keepSafe Nothing = return ()
          keepSafe (Just v) = do
              pushContext
              mapM_ delSymbol v

injectIntern :: FormulaPrim -> EqContext FormulaPrim
injectIntern f@(Variable v) =
    maybe f unTagFormula <$> symbolLookup v

injectIntern (Poly _ po@(Polynome _ _)) = checkPolynomeBinding' po

injectIntern f@(Meta _ Hold _) = return f
injectIntern f = scope $ reBoundVar f
    where scope Nothing = return f
          scope _ = popContext >> return f
                 
-- | Tell if a node change the scope.
-- The pattern is explicitely exaustive to be sure
-- to get the compiler shout if a change is made.
reBoundVar :: FormulaPrim -> Maybe [String]
reBoundVar (Product _ (BinOp _ OpEq (Variable v:_)) _ _) = Just [v]
reBoundVar (Sum _ (BinOp _ OpEq (Variable v: _)) _ _) = Just [v]
reBoundVar (Lambda _ clauses) = Just $
    concat [concatMap collectSymbols args | (args, _) <- clauses]

reBoundVar (Infer _ _ _) = Nothing
reBoundVar (Indexes _ _ _) = Nothing
reBoundVar (List _ _) = Nothing
reBoundVar (Display _ _) = Nothing
reBoundVar (Stack _ _) = Nothing
reBoundVar (Complex _ _) = Nothing
reBoundVar (Fraction _) = Nothing
reBoundVar (Poly _ _) = Nothing
reBoundVar (Variable _) = Nothing
reBoundVar (NumEntity _) = Nothing
reBoundVar (CInteger _) = Nothing
reBoundVar (CFloat _) = Nothing
reBoundVar (App _ _ _) = Nothing
reBoundVar (Derivate _ _ _) = Nothing
reBoundVar (Integrate _ _ _ _ _) = Nothing
reBoundVar (UnOp _ _ _) = Nothing
reBoundVar (BinOp _ _ _) = Nothing
reBoundVar (Matrix _ _ _ _) = Nothing
reBoundVar (Block _ _ _) = Nothing
reBoundVar (Product _ _ _ _) = Nothing
reBoundVar (Sum _ _ _ _) = Nothing
reBoundVar (Truth _) = Nothing
-- Nothing preserved during evaluation normaly.
reBoundVar (Meta _ _ _) = Nothing
