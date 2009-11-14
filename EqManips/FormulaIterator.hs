{-# LANGUAGE ScopedTypeVariables #-}
module EqManips.FormulaIterator( depthFirstFormula
                               , depthFormulaTraversal 
                               , depthFormulaPrimTraversal 
                               , depthPrimTraversal 
                               ) where

import EqManips.Types
import Control.Monad( mapM )

-- | Depth first traversal of formula.
-- the function is applied to each subformula when
-- the traversal is coming back to the top of the
-- formula tree.
depthFirstFormula :: (Monad m) 
                  => (Formula a -> m (Formula b)) -> Formula a -> m (Formula b)
depthFirstFormula = depthFormulaTraversal . const $ return ()

depthFormulaTraversal :: (Monad m) 
                      => (Formula a -> m ())
                      -> (Formula a -> m (Formula b))
                      -> Formula a -> m (Formula b)
depthFormulaTraversal pre f formula = do
    prim <- depthPrimTraversal
                      (pre . Formula)
                      -- Can't get it to compile with >>= or <$>
                      -- so back to ugly form
                      (\a -> do a' <- f $ Formula a
                                return $ unTagFormula a')
                      $ unTagFormula formula
    return $ Formula prim


depthFormulaPrimTraversal :: (Monad m)
                          => (FormulaPrim -> m FormulaPrim)
                          -> FormulaPrim
                          -> m FormulaPrim
depthFormulaPrimTraversal = depthPrimTraversal (const $ return ())

-- | Depth first traversal providing two events :
-- - One pre event which is called when a node is
--   reached when descending the tree
-- - One post event similar to depthFirstFormula,
--   reached when the traversal go up.
-- Note : the leaf don't have a pre event, just a
--        post.
depthPrimTraversal :: (Monad m) 
                   => (FormulaPrim -> m ()) 
                   -> (FormulaPrim -> m FormulaPrim)
                   -> FormulaPrim
                   -> m FormulaPrim
depthPrimTraversal _ f p@(Poly _) = f p
depthPrimTraversal _ f v@(Variable _) = f v
depthPrimTraversal _ f i@(CInteger _) = f i
depthPrimTraversal _ f d@(CFloat _) = f d
depthPrimTraversal _ f e@(NumEntity _) = f e
depthPrimTraversal _ f t@(Truth _) = f t
depthPrimTraversal pre f l@(Lambda eqs) = do
	pre l
	eqs' <- mapM traverser eqs
	f $ Lambda eqs'
		where traverser (args, body) = do
				body' <- depthPrimTraversal pre f body
				return (args, body')

depthPrimTraversal pre post meta@(Meta op f) = do
    pre meta
    f' <- depthPrimTraversal pre post f
    post $ Meta op f'

depthPrimTraversal pre post formula@(App func args) = do
    pre formula
    fs <- depthPrimTraversal pre post func
    argss <- mapM (depthPrimTraversal pre post) args
    post $ App fs argss

depthPrimTraversal pre post formula@(Sum ini end what) = do
    pre formula
    inis <- depthPrimTraversal pre post ini
    ends <- depthPrimTraversal pre post end
    whats <- depthPrimTraversal pre post what
    post $ Sum inis ends whats

depthPrimTraversal pre post formula@(Product ini end what) = do
    pre formula
    inis <- depthPrimTraversal pre post ini
    ends <- depthPrimTraversal pre post end
    whats <- depthPrimTraversal pre post what
    post $ Product inis ends whats

depthPrimTraversal pre post formula@(Derivate what var) = do
    pre formula
    whats <- depthPrimTraversal pre post what
    vars <- depthPrimTraversal pre post var
    post $ Derivate whats vars

depthPrimTraversal pre post formula@(Integrate ini end what var) = do
    pre formula
    inis <- depthPrimTraversal pre post ini
    ends <- depthPrimTraversal pre post end
    whats <- depthPrimTraversal pre post what
    vars <- depthPrimTraversal pre post var
    post $ Integrate inis ends whats vars

depthPrimTraversal pre post formula@(Matrix n m cells) = do
    pre formula
    cellss <- sequence [ mapM (depthPrimTraversal pre post) matrixLine
                            | matrixLine <- cells]
    post $ Matrix n m cellss

depthPrimTraversal pre post formula@(UnOp op sub) = do
    pre formula
    subs <- depthPrimTraversal pre post sub
    post $ UnOp op subs

depthPrimTraversal pre post formula@(BinOp op fs) = do
    pre formula
    fs' <- mapM (depthPrimTraversal pre post) fs
    post $ BinOp op fs'

-- Hmm, it's a debug for renderer, we dont really care
depthPrimTraversal _ _ b@(Block _ _ _) = return b

