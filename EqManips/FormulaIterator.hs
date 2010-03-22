{-# LANGUAGE ScopedTypeVariables #-}
module EqManips.FormulaIterator( depthFirstFormula
                               , depthFormulaTraversal 
                               , depthFormulaPrimTraversal 
                               , depthPrimTraversal 
                               , topDownTraversal 
                               ) where

import Control.Applicative
import EqManips.Types
import Data.Maybe( fromMaybe )
import Control.Monad( mapM )
import EqManips.EvaluationContext
import EqManips.Algorithm.EmptyMonad

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

-- | This function must be used to transform
topDownTraversal :: (FormulaPrim -> Maybe FormulaPrim)
                 -> FormulaPrim
                 -> FormulaPrim
topDownTraversal f p@(Poly _ _) = fromMaybe p $ f p
topDownTraversal f v@(Variable _) = fromMaybe v $ f v
topDownTraversal f i@(CInteger _) = fromMaybe i $ f i
topDownTraversal f i@(Fraction _) = fromMaybe i $ f i
topDownTraversal f i@(Complex _ _) = fromMaybe i $ f i
topDownTraversal f d@(CFloat _) = fromMaybe d $ f d
topDownTraversal f e@(NumEntity _) = fromMaybe e $ f e
topDownTraversal f t@(Truth _) = fromMaybe t $ f t
topDownTraversal f l@(Lambda _ eqs) = 
    fromMaybe (lambda lambda') $ f l
        where lambda' =
                  [ ( map (topDownTraversal f) args
                    , topDownTraversal f body) | (args, body) <- eqs]

topDownTraversal f met@(Meta _ op form) =
    fromMaybe (meta op $ topDownTraversal f form) $ f met

topDownTraversal f i@(Indexes _ what lst) =
    fromMaybe (indexes (topDownTraversal f what) $ map (topDownTraversal f) lst) $ f i

topDownTraversal f l@(List _ lst) =
    fromMaybe (list $ map (topDownTraversal f) lst) $ f l

topDownTraversal f formula@(App _ func args) =
    fromMaybe (app mayFunc mayArgs) $ f formula
        where mayFunc = topDownTraversal f func
              mayArgs = map (topDownTraversal f) args

topDownTraversal f formula@(Sum _ ini end what) =
    fromMaybe (summ mayIni mayEnd mayWhat) $ f formula
        where mayIni = topDownTraversal f ini
              mayEnd = topDownTraversal f end
              mayWhat = topDownTraversal f what

topDownTraversal f formula@(Product _ ini end what) =
    fromMaybe (productt mayIni mayEnd mayWhat) $ f formula
        where mayIni = topDownTraversal f ini
              mayEnd = topDownTraversal f end
              mayWhat = topDownTraversal f what

topDownTraversal f formula@(Derivate _ what var) =
    fromMaybe (derivate mayWhat mayVar ) $ f formula
        where mayVar = topDownTraversal f var
              mayWhat = topDownTraversal f what

topDownTraversal f formula@(Integrate _ ini end what var) =
    fromMaybe (integrate mayIni mayEnd mayWhat mayVar) $ f formula
        where mayIni = topDownTraversal f ini
              mayEnd = topDownTraversal f end
              mayWhat = topDownTraversal f what
              mayVar = topDownTraversal f var

topDownTraversal f formula@(Matrix _ n m cells) =
    fromMaybe (matrix n m [[topDownTraversal f cell | cell <- line] | line <- cells])
            $ f formula

topDownTraversal f formula@(UnOp _ op sub) =
    fromMaybe (unOp op $ topDownTraversal f sub) $ f formula

topDownTraversal f formula@(BinOp _ op fs) =
    fromMaybe (binOp op $ map (topDownTraversal f) fs) $ f formula

-- Hmm, it's a debug for renderer, we dont really care
topDownTraversal _ b@(Block _ _ _) = b



-- | Depth first traversal providing two events :
-- - One pre event which is called when a node is
--   reached when descending the tree
-- - One post event similar to depthFirstFormula,
--   reached when the traversal go up.
-- Note : the leaf don't have a pre event, just a
--        post.
{-# SPECIALIZE depthPrimTraversal :: (FormulaPrim -> EmptyMonad ())
                                  -> (FormulaPrim -> EmptyMonad FormulaPrim)
                                  -> FormulaPrim -> EmptyMonad FormulaPrim #-}
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

