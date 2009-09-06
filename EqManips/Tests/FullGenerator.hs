{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-methods #-}
module EqManips.Tests.FullGenerator( formulaGen ) where

import Control.Applicative
import Control.Monad
import Test.QuickCheck
import EqManips.Types

-- | To generate variables without clashing everywhere else
newtype VarLetter = VarLetter Char
instance Applicative Gen where
    pure = return
    a <*> b = do { a' <- a; b' <- b; return $ a' b' }

instance Arbitrary Entity where
    arbitrary  = elements [Pi] 

instance Arbitrary BinOperator where
    arbitrary  = elements [ OpAdd, OpSub, OpMul, OpDiv, OpPow, OpEq ]

instance Arbitrary UnOperator where
    arbitrary  = elements [ OpNegate, OpAbs, OpSqrt, OpSin, OpSinh
                          , OpASin, OpASinh, OpCos, OpCosh, OpACos
                          , OpACosh, OpTan, OpTanh, OpATan, OpATanh
                          , OpLn, OpLog, OpExp, OpFactorial
                          , OpCeil, OpFloor, OpFrac
                          ]

instance Arbitrary Formula where
    arbitrary = formulaGen 2


instance Arbitrary VarLetter where
    arbitrary = do
        n <- choose (0, 25)
        return . VarLetter . toEnum $ n + fromEnum 'a'

leafs :: [Gen Formula]
leafs = 
    [ liftM CInteger arbitrary
    -- Causing problem with equality check...
    -- Which is absolutely normal...
    {-, liftM CFloat arbitrary-}
    , liftM NumEntity arbitrary
    , liftM (\(VarLetter c) -> Variable [c]) arbitrary 
    ]

formulaGen :: Int -> Gen Formula
formulaGen n  
    | n <= 0 = oneof leafs
    | otherwise = oneof $
        leafs ++ [ BinOp <$> arbitrary <*> formulist
                 , UnOp <$> arbitrary <*> subFormul
                 , Sum <$> subFormul <*> subFormul <*> subFormul
                 , Product <$> subFormul <*> subFormul <*> subFormul
                 , Derivate <$> subFormul <*> subFormul
                 , Integrate <$> subFormul <*> subFormul 
                             <*> subFormul <*> subFormul
                 , App <$> subFormul <*> formulist
                 , matrixGenerator (n-1)
                 ]
          where subFormul = formulaGen (n-1)
                formulist = do f1 <- subFormul
                               f2 <- subFormul
                               return [f1, f2]

matrixGenerator :: Int -> Gen Formula
matrixGenerator deep = do
    n <- choose (1, 5)
    m <- choose (1, 5)
    let subf = formulaGen $ deep - 1
    eqs <- replicateM m $ replicateM n subf
    return $ Matrix n m eqs

