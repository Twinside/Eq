{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-methods #-}
module EqManips.Tests.FullGenerator( formulaGen ) where

import Data.Ratio
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

instance Arbitrary FormulaPrim where
    arbitrary = formulaGen 1

instance Arbitrary PolyCoeff where
    arbitrary = do
        n :: Int <- choose (0,1)
        case n of
             0 -> CoeffInt <$> choose (1,150)
             1 -> (\a b -> CoeffRatio $ a % b) <$> choose (1,150) <*> choose (1,150)
             _ -> error "Not permited"

instance Arbitrary VarLetter where
    arbitrary = do
        n <- choose (0, 25)
        return . VarLetter . toEnum $ n + fromEnum 'a'

leafs :: [Gen FormulaPrim]
leafs = 
    [ liftM CInteger arbitrary
    -- Causing problem with equality check...
    -- Which is absolutely normal...
    {-, liftM CFloat arbitrary-}
    , liftM NumEntity arbitrary
    , liftM (\(VarLetter c) -> Variable [c]) arbitrary 
    ]

formulaGen :: Int -> Gen FormulaPrim
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
                 , Poly <$> polynomeGenerator (n-1)
                 , matrixGenerator (n-1)
                 ]
          where subFormul = formulaGen (n-1)
                formulist = do genCount <- choose (2, 7)
                               replicateM genCount subFormul

polynomeGenerator :: Int -> Gen Polynome
polynomeGenerator i | i < 0 = PolyRest <$> arbitrary
polynomeGenerator i = do
    end :: Int <- choose (1,2)
    if end == 1
       then PolyRest <$> arbitrary
       else do
        count <- choose (1,10)
        body <- replicateM count ((,) <$> arbitrary <*> polynomeGenerator (i-1))
        (VarLetter v) <- arbitrary
        return $ Polynome [v] body

matrixGenerator :: Int -> Gen FormulaPrim
matrixGenerator deep = do
    n <- choose (1, 5)
    m <- choose (1, 5)
    let subf = formulaGen $ deep - 1
    eqs <- replicateM m $ replicateM n subf
    return $ Matrix n m eqs

