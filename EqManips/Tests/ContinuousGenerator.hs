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
newtype ContinuousFormula = ContinuousFormula Formula

instance Arbitrary ContinuousEntity where
    arbitrary  = elements [ Pi ] 
               >>= return . ContinuousEntity 

instance Arbitrary ContinuousBinop where
    arbitrary  = elements [ OpAdd, OpSub, OpMul, OpDiv, OpPow ] 
               >>= return . ContinuousBinop

instance Arbitrary ContinuousUnop where
    arbitrary  = elements [ OpNegate, OpSqrt, OpSin, OpCos, OpTan, OpLn, OpExp ]
               >>= return . ContinuousUnop 

instance Arbitrary ContinuousFormula where
    arbitrary = formulaGen 5

instance Applicative Gen where
    pure  = return 
    f <*> a = do f' <- f
                 a' <- a
                 return $ f' a'
    
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
    leafs ++ [ ContinuousFormula <$> (UnOp <$> arbunop <*> subFormul)
             , ContinuousFormula <$> (BinOp <$> arbinop <*> sequence [subFormul, subFormul])
             ]
      where subFormul = do
                ContinuousFormula f <- formulaGen (n-1)
                return f

