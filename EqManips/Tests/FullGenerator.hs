
module EqManips.Tests.FullGenerator( formulaGen ) where

import Control.Monad
import Test.QuickCheck
import EqManips.Types

instance Arbitrary Entity where
    arbitrary  = elements [Pi] 

instance Arbitrary BinOperator where
    arbitrary  = elements [ OpAdd, OpSub, OpMul, OpDiv, OpPow, OpEq ]

instance Arbitrary UnOperator where
    arbitrary  = elements [ OpNegate, OpAbs, OpSqrt, OpSin, OpSinh
                          , OpASin, OpASinh, OpCos, OpCosh, OpACos
                          , OpACosh, OpTan, OpTanh, OpATan, OpATanh
                          , OpLn, OpLog, OpExp]

instance Arbitrary Formula where
    arbitrary = formulaGen 1

instance Arbitrary Char where
    arbitrary = do
        n <- choose (0, 25)
        return . toEnum $ n + fromEnum 'a'

leafs :: [Gen Formula]
leafs = 
    [ liftM CInteger arbitrary
    -- Causing problem with equality check...
    -- Which is absolutely normal...
    {-, liftM CFloat arbitrary-}
    , liftM NumEntity arbitrary
    , liftM (Variable . (:[])) arbitrary 
    ]

formulaGen :: Int -> Gen Formula
formulaGen n  
    | n <= 0 = oneof leafs
    | otherwise = oneof $
        leafs ++ [ liftM3 BinOp arbitrary subFormul subFormul
                 , liftM2 UnOp arbitrary subFormul
                 , liftM3 Sum subFormul subFormul subFormul
                 , liftM3 Product subFormul subFormul subFormul
                 , liftM2 Derivate subFormul subFormul
                 , liftM4 Integrate subFormul subFormul subFormul subFormul
                 , matrixGenerator (n-1)
                 ]
          where subFormul = formulaGen (n-1)

matrixGenerator :: Int -> Gen Formula
matrixGenerator deep = do
    n <- choose (1, 5)
    m <- choose (1, 5)
    let subf = formulaGen $ deep - 1
    eqs <- replicateM m $ replicateM n subf
    return $ Matrix n m eqs

