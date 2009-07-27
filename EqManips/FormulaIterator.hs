module EqManips.FormulaIterator where

import EqManips.Types
formulaIterate :: (Monad m) 
               => (Formula -> m Formula) -> Formula -> m Formula
formulaIterate f v@(Variable _) = f v
formulaIterate f i@(CInteger _) = f i
formulaIterate f d@(CFloat _) = f d
formulaIterate f e@(NumEntity _) = f e

formulaIterate f (App func args) = do
    fs <- f func
    argss <- mapM f args
    return $ App fs argss

formulaIterate f (Sum ini end what) = do
    inis <- f ini
    ends <- f end
    whats <- f what
    return $ Sum inis ends whats

formulaIterate f (Product ini end what) = do
    inis <- f ini
    ends <- f end
    whats <- f what
    return $ Sum inis ends whats

formulaIterate f (Derivate what var) = do
    whats <- f what
    vars <- f var
    return $ Derivate whats vars

formulaIterate f (Integrate ini end what var) = do
    inis <- f ini
    ends <- f end
    whats <- f what
    vars <- f var
    return $ Integrate inis ends whats vars

formulaIterate f (Matrix n m cells) = do
    cellss <- sequence [mapM f matrixLine | matrixLine <- cells]
    return $ Matrix n m cellss

formulaIterate f (UnOp op sub) = do
    subs <- f sub
    return $ UnOp op subs

formulaIterate f (BinOp op f1 f2) = do
    f1s <- f f1
    f2s <- f f2
    return $ BinOp op f1s f2s

-- Hmm, it's a debug for renderer, we dont really care
formulaIterate _ b@(Block _ _ _) = return b

