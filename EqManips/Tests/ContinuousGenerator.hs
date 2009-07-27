module EqManips.Tests.ContinuousGenerator( formulaGen ) where

import Control.Monad
import Test.QuickCheck
import EqManips.Types

instance Arbitrary Entity where
    arbitrary  = elements [Pi, Nabla] 

instance Arbitrary BinOperator where
    arbitrary  = elements [ OpAdd, OpSub, OpMul, OpDiv, OpPow ]

instance Arbitrary UnOperator where
    arbitrary  = elements [ OpNegate, OpSqrt, OpSin, OpCos, OpTan, OpLn, OpExp ]

instance Arbitrary Formula where
    arbitrary = formulaGen 5

leafs :: [Gen Formula]
leafs = 
    [ liftM NumEntity arbitrary
    , liftM CInteger arbitrary
    , liftM CFloat arbitrary ]

formulaGen :: Int -> Gen Formula
formulaGen 0 = oneof leafs
formulaGen n = oneof $
    leafs ++ [ liftM3 BinOp arbitrary subFormul subFormul
             , liftM2 UnOp arbitrary subFormul
             ]
      where subFormul = formulaGen (n-1)

