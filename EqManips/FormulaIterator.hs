{-# LANGUAGE ScopedTypeVariables #-}
module EqManips.FormulaIterator( depthFirstFormula
                               , depthFormulaTraversal 
                               , depthFormulaPrimTraversal 
                               , depthPrimTraversal 
                               , topDownTraversal 
                               ) where

import EqManips.Types
import Data.Maybe( fromMaybe )
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

-- | This function must be used to transform
topDownTraversal :: (FormulaPrim -> Maybe FormulaPrim)
                 -> FormulaPrim
                 -> FormulaPrim
topDownTraversal f p@(Poly _) = fromMaybe p $ f p
topDownTraversal f v@(Variable _) = fromMaybe v $ f v
topDownTraversal f i@(CInteger _) = fromMaybe i $ f i
topDownTraversal f i@(Fraction _) = fromMaybe i $ f i
topDownTraversal f i@(Complex _) = fromMaybe i $ f i
topDownTraversal f d@(CFloat _) = fromMaybe d $ f d
topDownTraversal f e@(NumEntity _) = fromMaybe e $ f e
topDownTraversal f t@(Truth _) = fromMaybe t $ f t
topDownTraversal f l@(Lambda eqs) = 
    fromMaybe (Lambda lambda') $ f l
        where lambda' =
                  [ ( map (topDownTraversal f) args
                    , topDownTraversal f body) | (args, body) <- eqs]

topDownTraversal f meta@(Meta op form) =
    fromMaybe (Meta op $ topDownTraversal f form) $ f meta

topDownTraversal f formula@(App func args) =
    fromMaybe (App mayFunc mayArgs) $ f formula
        where mayFunc = topDownTraversal f func
              mayArgs = map (topDownTraversal f) args

topDownTraversal f formula@(Sum ini end what) =
    fromMaybe (Sum mayIni mayEnd mayWhat) $ f formula
        where mayIni = topDownTraversal f ini
              mayEnd = topDownTraversal f end
              mayWhat = topDownTraversal f what

topDownTraversal f formula@(Product ini end what) =
    fromMaybe (Product mayIni mayEnd mayWhat) $ f formula
        where mayIni = topDownTraversal f ini
              mayEnd = topDownTraversal f end
              mayWhat = topDownTraversal f what

topDownTraversal f formula@(Derivate what var) =
    fromMaybe (Derivate mayWhat mayVar ) $ f formula
        where mayVar = topDownTraversal f var
              mayWhat = topDownTraversal f what

topDownTraversal f formula@(Integrate ini end what var) =
    fromMaybe (Integrate mayIni mayEnd mayWhat mayVar) $ f formula
        where mayIni = topDownTraversal f ini
              mayEnd = topDownTraversal f end
              mayWhat = topDownTraversal f what
              mayVar = topDownTraversal f var

topDownTraversal f formula@(Matrix n m cells) =
    fromMaybe (Matrix n m [[topDownTraversal f cell | cell <- line] | line <- cells])
            $ f formula

topDownTraversal f formula@(UnOp op sub) =
    fromMaybe (UnOp op $ topDownTraversal f sub) $ f formula

topDownTraversal f formula@(BinOp op fs) =
    fromMaybe (BinOp op $ map (topDownTraversal f) fs) $ f formula

-- Hmm, it's a debug for renderer, we dont really care
topDownTraversal _ b@(Block _ _ _) = b



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
depthPrimTraversal _ f i@(Fraction _) = f i
depthPrimTraversal _ f i@(Complex _) = f i
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

