module EqManips.Tests.ContinuousGenerator( ContinuousFormula( .. )
                                         , formulaGen 
                                         ) where

import Control.Monad
import Test.QuickCheck
import EqManips.Types

newtype ContinuousBinop = ContinuousBinop BinOperator
newtype ContinuousUnop = ContiousUnop UnOperator
newtype ContinuousFormula = ContinousFormula Formula

instance Arbitrary Entity where
    arbitrary  = elements [ Pi ] 

instance Arbitrary ContinuousBinop where
    arbitrary  = elements [ OpAdd, OpSub, OpMul, OpDiv, OpPow ] 
               >>= return . ContinuousBinop

instance Arbitrary UnOperator where
    arbitrary  = elements [ OpNegate, OpSqrt, OpSin, OpCos, OpTan, OpLn, OpExp ]
               >>= return . ContiousUnop 

instance Arbitrary ContinousFormula where
    arbitrary = formulaGen 5

leafs :: [Gen ContinousFormula]
leafs = 
    [ liftM (ContinuousFormula . NumEntity) arbitrary
    , liftM (CInteger . ContinuousFormula) arbitrary
    , liftM (ContinuousFormula . CFloat) arbitrary
    , return . ContinuousFormula $ Variable "x"
    ]

formulaGen :: Int -> Gen ContinousFormula
formulaGen 0 = oneof leafs >>= return . ContinousFormula
formulaGen n = oneof $
    leafs ++ [ liftM3 (ContinuousFormula . BinOp) arbitrary subFormul subFormul
             , liftM2 (ContinuousFormula . UnOp) arbitrary subFormul
             ]
      where subFormul = do
                ContinuousFormula f <- formulaGen (n-1)
                return f

