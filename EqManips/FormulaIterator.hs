{-# LANGUAGE ScopedTypeVariables #-}
module EqManips.FormulaIterator( depthFirstFormula
                               , depthFormulaTraversal 
                               , depthFormulaPrimTraversal 
                               , depthPrimTraversal 
                               , topDownTraversal 
                               , topDownScanning 
                               ) where

import Control.Applicative
import Control.Monad.Identity
import EqManips.Types

import EqManips.EvaluationContext

-- | Depth first traversal of formula.
-- the function is applied to each subformula when
-- the traversal is coming back to the top of the
-- formula tree.
depthFirstFormula :: (Applicative m, Monad m) 
                  => (Formula a -> m (Formula b)) -> Formula a -> m (Formula b)
depthFirstFormula = depthFormulaTraversal . const $ return ()

depthFormulaTraversal :: (Applicative m, Monad m)
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


depthFormulaPrimTraversal :: (Applicative m, Monad m)
                          => (FormulaPrim -> m FormulaPrim)
                          -> FormulaPrim
                          -> m FormulaPrim
depthFormulaPrimTraversal = depthPrimTraversal (const $ return ())

topDownTraversal :: (FormulaPrim -> Maybe FormulaPrim)
                 -> FormulaPrim -> FormulaPrim
topDownTraversal f formu =
    runIdentity $ topDownScanning (return . f) formu

fromMaybeM :: (Monad m) => m a -> m (Maybe a) -> m a
fromMaybeM e da = do
    rez <- da
    case rez of
         Nothing -> e
         Just a  -> return a

-- | This function must be used to transform function from
-- the top.
{-# SPECIALIZE topDownScanning :: (FormulaPrim -> Identity (Maybe FormulaPrim))
                               -> FormulaPrim -> Identity FormulaPrim #-}
{-# SPECIALIZE topDownScanning :: (FormulaPrim -> EqContext (Maybe FormulaPrim))
                               -> FormulaPrim -> EqContext FormulaPrim #-}
topDownScanning :: (Monad m, Applicative m)
                => (FormulaPrim -> m (Maybe FormulaPrim))
                -> FormulaPrim
                -> m FormulaPrim
topDownScanning f p@(Poly _ _) = fromMaybeM (return p) $ f p
topDownScanning f v@(Variable _) = fromMaybeM (return v) $ f v
topDownScanning f i@(CInteger _) = fromMaybeM (return i) $ f i
topDownScanning f i@(Fraction _) = fromMaybeM (return i) $ f i
topDownScanning f i@(Complex _ _) = fromMaybeM (return i) $ f i
topDownScanning f d@(CFloat _) = fromMaybeM (return d) $ f d
topDownScanning f e@(NumEntity _) = fromMaybeM (return e) $ f e
topDownScanning f t@(Truth _) = fromMaybeM (return t) $ f t
topDownScanning f l@(Lambda _ eqs) = 
    fromMaybeM (lambda <$> lambda') $ f l
        where lambda' = sequence
                  [ do args' <- mapM (topDownScanning f) args
                       body' <- topDownScanning f body
                       return (args', body') | (args, body) <- eqs]

topDownScanning f met@(Meta _ op form) =
    fromMaybeM (meta op <$> topDownScanning f form) $ f met

topDownScanning f i@(Indexes _ what lst) = do
    what' <- topDownScanning f what
    fromMaybeM (indexes what' <$> mapM (topDownScanning f) lst)
                 $ f i

topDownScanning f l@(List _ lst) =
    fromMaybeM (list <$> mapM (topDownScanning f) lst) $ f l

topDownScanning f formula@(App _ func args) =
    fromMaybeM (app <$> mayFunc <*> mayArgs) $ f formula
        where mayFunc = topDownScanning f func
              mayArgs = mapM (topDownScanning f) args

topDownScanning f formula@(Sum _ ini end what) =
    fromMaybeM (summ <$> mayIni <*> mayEnd <*> mayWhat) $ f formula
        where mayIni = topDownScanning f ini
              mayEnd = topDownScanning f end
              mayWhat = topDownScanning f what

topDownScanning f formula@(Product _ ini end what) =
    fromMaybeM (productt <$> mayIni <*> mayEnd <*> mayWhat) $ f formula
        where mayIni = topDownScanning f ini
              mayEnd = topDownScanning f end
              mayWhat = topDownScanning f what

topDownScanning f formula@(Derivate _ what var) =
    fromMaybeM (derivate <$> mayWhat <*> mayVar ) $ f formula
        where mayVar = topDownScanning f var
              mayWhat = topDownScanning f what

topDownScanning f formula@(Integrate _ ini end what var) =
    fromMaybeM (integrate <$> mayIni <*> mayEnd <*> mayWhat <*> mayVar) $ f formula
        where mayIni = topDownScanning f ini
              mayEnd = topDownScanning f end
              mayWhat = topDownScanning f what
              mayVar = topDownScanning f var

topDownScanning f formula@(Matrix _ n m cells) =
    fromMaybeM (matrix n m <$> mapM (mapM (topDownScanning f)) cells)
            $ f formula

topDownScanning f formula@(UnOp _ op sub) =
    fromMaybeM (unOp op <$> topDownScanning f sub) $ f formula

topDownScanning f formula@(BinOp _ op fs) =
    fromMaybeM (binOp op <$> mapM (topDownScanning f) fs) $ f formula

-- Hmm, it's a debug for renderer, we dont really care
topDownScanning _ b@(Block _ _ _) = return b


-- | Depth first traversal providing two events :
-- - One pre event which is called when a node is
--   reached when descending the tree
-- - One post event similar to depthFirstFormula,
--   reached when the traversal go up.
-- Note : the leaf don't have a pre event, just a
--        post.
{-# SPECIALIZE depthPrimTraversal :: (FormulaPrim -> Identity ())
                                  -> (FormulaPrim -> Identity FormulaPrim)
                                  -> FormulaPrim -> Identity FormulaPrim #-}
{-# SPECIALIZE depthPrimTraversal :: (FormulaPrim -> EqContext ())
                                  -> (FormulaPrim -> EqContext FormulaPrim)
                                  -> FormulaPrim -> EqContext FormulaPrim #-}
depthPrimTraversal :: (Applicative m, Monad m) 
                   => (FormulaPrim -> m ()) 
                   -> (FormulaPrim -> m FormulaPrim)
                   -> FormulaPrim
                   -> m FormulaPrim
depthPrimTraversal _ f p@(Poly _ _) = f p
depthPrimTraversal _ f v@(Variable _) = f v
depthPrimTraversal _ f i@(CInteger _) = f i
depthPrimTraversal _ f i@(Fraction _) = f i
depthPrimTraversal _ f d@(CFloat _) = f d
depthPrimTraversal _ f e@(NumEntity _) = f e
depthPrimTraversal _ f t@(Truth _) = f t
depthPrimTraversal pre f i@(Indexes _ main lst) = do
    pre i
    main' <- depthPrimTraversal pre f main
    lst' <- mapM (depthPrimTraversal pre f) lst
    f $ indexes main' lst'

depthPrimTraversal pre f i@(List _ lst) = do
    pre i
    lst' <- mapM (depthPrimTraversal pre f) lst
    f $ list lst'

depthPrimTraversal pre f c@(Complex _ (r, i)) = do
    pre c
    r' <- depthPrimTraversal pre f r
    i' <- depthPrimTraversal pre f i
    f $ complex (r', i')

depthPrimTraversal pre f l@(Lambda _ eqs) = do
	pre l
	f =<< lambda <$> mapM traverser eqs
		where traverser (args, body) = do
				body' <- depthPrimTraversal pre f body
				return (args, body')

depthPrimTraversal pre post met@(Meta _ op f) = do
    pre met
    post =<< meta op <$> depthPrimTraversal pre post f

depthPrimTraversal pre post formula@(App _ func args) = do
    pre formula
    post =<< app <$> depthPrimTraversal pre post func
                 <*> mapM (depthPrimTraversal pre post) args

depthPrimTraversal pre post formula@(Sum _ ini end what) = do
    pre formula
    post =<< summ <$> depthPrimTraversal pre post ini
                  <*> depthPrimTraversal pre post end
                  <*> depthPrimTraversal pre post what

depthPrimTraversal pre post formula@(Product _ ini end what) = do
    pre formula
    post =<< productt <$> depthPrimTraversal pre post ini
                      <*> depthPrimTraversal pre post end
                      <*> depthPrimTraversal pre post what

depthPrimTraversal pre post formula@(Derivate _ what var) = do
    pre formula
    post =<< derivate <$> depthPrimTraversal pre post what
                      <*> depthPrimTraversal pre post var

depthPrimTraversal pre post formula@(Integrate _ ini end what var) = do
    pre formula
    post =<< integrate 
        <$> depthPrimTraversal pre post ini
        <*> depthPrimTraversal pre post end
        <*> depthPrimTraversal pre post what
        <*> depthPrimTraversal pre post var

depthPrimTraversal pre post formula@(Matrix _ n m cells) = do
    pre formula
    post =<< matrix n m
         <$> sequence [ mapM (depthPrimTraversal pre post) matrixLine
                            | matrixLine <- cells]

depthPrimTraversal pre post formula@(UnOp _ op sub) = do
    pre formula
    post =<< unOp op <$> depthPrimTraversal pre post sub

depthPrimTraversal pre post formula@(BinOp _ op fs) = do
    pre formula
    post =<< binOp op <$> mapM (depthPrimTraversal pre post) fs

-- Hmm, it's a debug for renderer, we dont really care
depthPrimTraversal _ _ b@(Block _ _ _) = return b

