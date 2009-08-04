module EqManips.FormulaIterator where

import EqManips.Types

-- | Depth first traversal of formula.
-- Be sure to _NOT_ call formulaIterate on leaf nodes,
-- you can use EqManips.Types.isFormulaLeaf to avoid this...
formulaIterate :: (Monad m) 
               => (Formula -> m Formula) -> Formula -> m Formula
formulaIterate f v@(Variable _) = f v
formulaIterate f i@(CInteger _) = f i
formulaIterate f d@(CFloat _) = f d
formulaIterate f e@(NumEntity _) = f e

formulaIterate f (App func args) = do
    fs <- formulaIterate f func
    argss <- mapM (formulaIterate f) args
    f $ App fs argss

formulaIterate f (Sum ini end what) = do
    inis <- formulaIterate f ini
    ends <- formulaIterate f end
    whats <- formulaIterate f what
    f $ Sum inis ends whats

formulaIterate f (Product ini end what) = do
    inis <- formulaIterate f ini
    ends <- formulaIterate f end
    whats <- formulaIterate f what
    f $ Product inis ends whats

formulaIterate f (Derivate what var) = do
    whats <- formulaIterate f what
    vars <- formulaIterate f var
    f $ Derivate whats vars

formulaIterate f (Integrate ini end what var) = do
    inis <- formulaIterate f ini
    ends <- formulaIterate f end
    whats <- formulaIterate f what
    vars <- formulaIterate f var
    f $ Integrate inis ends whats vars

formulaIterate f (Matrix n m cells) = do
    cellss <- sequence [mapM (formulaIterate f) matrixLine | matrixLine <- cells]
    f $ Matrix n m cellss

formulaIterate f (UnOp op sub) = do
    subs <- formulaIterate f sub
    f $ UnOp op subs

formulaIterate f (BinOp op f1 f2) = do
    f1s <- formulaIterate f f1
    f2s <- formulaIterate f f2
    f $ BinOp op f1s f2s

-- Hmm, it's a debug for renderer, we dont really care
formulaIterate _ b@(Block _ _ _) = return b

