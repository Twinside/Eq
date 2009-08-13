{-# LANGUAGE ScopedTypeVariables #-}
module EqManips.FormulaIterator where

import Data.List
import Data.Monoid( Monoid( .. ) )
import EqManips.Types
import Control.Monad( mapM )

-- | Depth first traversal of formula.
-- the function is applied to each subformula when
-- the traversal is coming back to the top of the
-- formula tree.
depthFirstFormula :: (Monad m) 
                  => (Formula -> m Formula) -> Formula -> m Formula
depthFirstFormula = depthFormulaTraversal . const $ return ()

-- | Depth first traversal providing two events :
-- - One pre event which is called when a node is
--   reached when descending the tree
-- - One post event similar to depthFirstFormula,
--   reached when the traversal go up.
-- Note : the leaf don't have a pre event, just a
--        post.
depthFormulaTraversal :: (Monad m) 
                      => (Formula -> m ())
                      -> (Formula -> m Formula)
                      -> Formula
                      -> m Formula
depthFormulaTraversal _ f v@(Variable _) = f v
depthFormulaTraversal _ f i@(CInteger _) = f i
depthFormulaTraversal _ f d@(CFloat _) = f d
depthFormulaTraversal _ f e@(NumEntity _) = f e
depthFormulaTraversal _ f l@(Lambda _) = f l

depthFormulaTraversal pre post meta@(Meta op f) = do
    pre meta
    f' <- depthFormulaTraversal pre post f
    post $ Meta op f'

depthFormulaTraversal pre post formula@(App func args) = do
    pre formula
    fs <- depthFormulaTraversal pre post func
    argss <- mapM (depthFormulaTraversal pre post) args
    post $ App fs argss

depthFormulaTraversal pre post formula@(Sum ini end what) = do
    pre formula
    inis <- depthFormulaTraversal pre post ini
    ends <- depthFormulaTraversal pre post end
    whats <- depthFormulaTraversal pre post what
    post $ Sum inis ends whats

depthFormulaTraversal pre post formula@(Product ini end what) = do
    pre formula
    inis <- depthFormulaTraversal pre post ini
    ends <- depthFormulaTraversal pre post end
    whats <- depthFormulaTraversal pre post what
    post $ Product inis ends whats

depthFormulaTraversal pre post formula@(Derivate what var) = do
    pre formula
    whats <- depthFormulaTraversal pre post what
    vars <- depthFormulaTraversal pre post var
    post $ Derivate whats vars

depthFormulaTraversal pre post formula@(Integrate ini end what var) = do
    pre formula
    inis <- depthFormulaTraversal pre post ini
    ends <- depthFormulaTraversal pre post end
    whats <- depthFormulaTraversal pre post what
    vars <- depthFormulaTraversal pre post var
    post $ Integrate inis ends whats vars

depthFormulaTraversal pre post formula@(Matrix n m cells) = do
    pre formula
    cellss <- sequence [ mapM (depthFormulaTraversal pre post) matrixLine
                            | matrixLine <- cells]
    post $ Matrix n m cellss

depthFormulaTraversal pre post formula@(UnOp op sub) = do
    pre formula
    subs <- depthFormulaTraversal pre post sub
    post $ UnOp op subs

depthFormulaTraversal pre post formula@(BinOp op fs) = do
    pre formula
    fs' <- mapM (depthFormulaTraversal pre post) fs
    post $ BinOp op fs'

-- Hmm, it's a debug for renderer, we dont really care
depthFormulaTraversal _ _ b@(Block _ _ _) = return b

foldf :: (Monoid b) => (Formula -> b -> b) -> b -> Formula -> b
foldf f acc m@(Meta _ fo) = f m $ foldf f acc fo
foldf f acc fo@(UnOp _ sub) = f fo $ foldf f acc sub
foldf f acc fo@(App def args) =
    foldf f (foldf f listAcc def) fo
     where listAcc = foldr f acc args

foldf f acc fo@(BinOp _ args) =
    f fo $ foldr f acc args

foldf f acc fo@(Sum ini end what) = f fo finalAcc
    where whatAcc = foldf f acc what
          iniAcc = foldf f acc ini
          endAcc = foldf f acc end
          finalAcc = whatAcc `mappend` iniAcc `mappend` endAcc

foldf f acc fo@(Product ini end what) = f fo finalAcc
        where whatAcc = foldf f acc what
              iniAcc = foldf f acc ini
              endAcc = foldf f acc end
              finalAcc = whatAcc `mappend` iniAcc `mappend` endAcc

foldf f acc fo@(Integrate ini end what var) = f fo finalAcc
        where whatAcc = foldf f acc what
              iniAcc = foldf f acc ini
              endAcc = foldf f acc end
              varAcc = foldf f acc var
              finalAcc = whatAcc `mappend` iniAcc 
                                 `mappend` endAcc `mappend` varAcc

foldf f acc fo@(Derivate what var) = f fo $ whatAcc `mappend` varAcc
        where whatAcc = foldf f acc what
              varAcc = foldf f acc var

foldf f acc fo@(Matrix _ _ cells) = f fo finalAcc
    where lineFolder acc' formu = acc' `mappend` (foldf f acc formu)
          rowAccs = [ foldl' lineFolder mempty row | row <- cells]
          finalAcc = foldl1' mappend rowAccs

foldf f acc fo = f fo acc

