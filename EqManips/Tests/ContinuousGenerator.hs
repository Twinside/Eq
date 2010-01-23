{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-methods #-}
module EqManips.Tests.ContinuousGenerator( ContinuousFormula( .. )
                                         , ContinuousBinop( .. )
                                         , ContinuousUnop( .. )
                                         , formulaGen 
                                         ) where

import Control.Applicative
import Control.Monad
import Test.QuickCheck
import EqManips.Types

newtype ContinuousEntity = ContinuousEntity Entity
newtype ContinuousBinop = ContinuousBinop BinOperator
newtype ContinuousUnop = ContinuousUnop UnOperator
newtype ContinuousFormula = ContinuousFormula FormulaPrim

instance Arbitrary ContinuousEntity where
    arbitrary  = ContinuousEntity <$> elements [ Pi ] 

instance Arbitrary ContinuousBinop where
    arbitrary  = ContinuousBinop <$> elements [ OpAdd, OpSub, OpMul, OpDiv, OpPow ] 

instance Arbitrary ContinuousUnop where
    arbitrary  = ContinuousUnop <$> elements [ OpNegate, OpSqrt, OpSin, OpCos, OpTan, OpLn, OpExp ]

instance Arbitrary ContinuousFormula where
    arbitrary = formulaGen 5

{-
-- Declared in last version apparently
instance Applicative Gen where
    pure  = return 
    f <*> a = do f' <- f
                 a' <- a
                 return $ f' a'
-}
    
arbunop :: Gen UnOperator
arbunop = arbitrary >>= \(ContinuousUnop op) -> return op

arbinop :: Gen BinOperator
arbinop = arbitrary >>= \(ContinuousBinop op) -> return op

arbentit :: Gen Entity
arbentit = arbitrary >>= \(ContinuousEntity op) -> return op

leafs :: [Gen ContinuousFormula]
leafs = 
    [ ContinuousFormula . NumEntity <$> arbentit
    , liftM (ContinuousFormula . CInteger) arbitrary
    , liftM (ContinuousFormula . CFloat) arbitrary
    , return . ContinuousFormula $ Variable "x"
    ]

formulaGen :: Int -> Gen ContinuousFormula
formulaGen 0 = oneof leafs
formulaGen n = oneof $
    leafs ++ [ ContinuousFormula <$> (unOp <$> arbunop <*> subFormul)
             , ContinuousFormula <$> (binOp <$> arbinop <*> sequence [subFormul, subFormul])
             ]
      where subFormul = do
                ContinuousFormula f <- formulaGen (n-1)
                return f

