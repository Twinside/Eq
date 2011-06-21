module Language.Eq.Algorithm.Simplify( simplifyFormula ) where

import Control.Applicative
import Data.Ratio
import Data.Maybe( mapMaybe )

import Language.Eq.Types
import Language.Eq.EvaluationContext
import Language.Eq.Algorithm.Eval.Utils
import Language.Eq.Algorithm.Eval.Types

#ifdef _DEBUG
import Language.Eq.Algorithm.Utils

tracer :: String -> BinOperator -> FormulaPrim -> FormulaPrim
       -> EqContext ()
tracer str op f1 f2 =
  addTrace (str, treeIfyFormula . Formula 
                                 $ binOp op [ f1, f2 ])
#endif

--------------------------------------------------
----            Operators
--------------------------------------------------

-- | '+' operator simplification.
-- Some propreties which should work for the addition
-- operation.
addSimplification :: EvalFun -> EvalOp
#ifdef _DEBUG
addSimplification eval a second@(BinOp _ OpMul [b, c])
#else
addSimplification eval a (BinOp _ OpMul [b, c])
#endif
    | hashOfFormula a == hashOfFormula c 
        && a == c = do
#ifdef _DEBUG
        tracer "Triggered '+' simplification" OpAdd a second
#endif
        subCoeff <- eval $ b + 1
        left $ subCoeff * c

#ifdef _DEBUG
addSimplification eval first@(BinOp _ OpMul [a, c]) b
#else
addSimplification eval (BinOp _ OpMul [a, c]) b
#endif
    | hashOfFormula c == hashOfFormula b 
        && b == c = do
#ifdef _DEBUG
        tracer "Triggered '+' simplification" OpAdd first b
#endif
        subCoeff <- eval $ a + 1
        left $ subCoeff * c
addSimplification _ a b
    | hashOfFormula a == hashOfFormula b
        && a == b = 
#ifdef _DEBUG
        tracer "Triggered '+' simplification" OpAdd a b >>
#endif
        left (2 * a)
    | otherwise = right $ (a,b)

-- | '-' operator simplification
subSimplification :: EvalFun -> EvalOp
{-subSimplification eval (Variable v) (BinOp _ OpDiv [a, somethingWithV])-}

{- if c == b  then a * c - b = (a-1) * c -}
#ifdef _DEBUG
subSimplification eval first@(BinOp _ OpMul [a, c]) b
#else
subSimplification eval (BinOp _ OpMul [a, c]) b
#endif
    | hashOfFormula c == hashOfFormula b 
        && b == c = do
#ifdef _DEBUG
        tracer "Triggered '-' simplification" OpSub first b
#endif
        subCoeff <- eval (a - 1)
        left (subCoeff * c)

subSimplification _ a b
    | hashOfFormula a == hashOfFormula b
        && a == b = 
#ifdef _DEBUG
        tracer "Triggered '-' simplification" OpSub a b >>
#endif
        left 0
    | otherwise = right (a,b)

--------------------------------------------------
----            '*' simplification
--------------------------------------------------
mulSimplification :: EvalFun -> EvalOp
mulSimplification eval (BinOp _ OpPow [a, c]) b
    | hashOfFormula a == hashOfFormula b
        && a == b = 
#ifdef _DEBUG
        tracer "Triggered '*' simplification" OpMul a b >>
#endif
        Left <$> eval (a ** (c + 1))

mulSimplification eval b (BinOp _ OpPow [a, c])
    | hashOfFormula a == hashOfFormula b
        && a == b = 
#ifdef _DEBUG
        tracer "Triggered '*' simplification" OpMul b a >>
#endif
        Left <$> eval (a ** (c + 1))

mulSimplification _ a b
    | hashOfFormula a == hashOfFormula b
        && a == b =
#ifdef _DEBUG
        tracer "Triggered '*' simplification" OpMul a b >>
#endif
        left (a ** 2)
    | otherwise = right (a,b)

--------------------------------------------------
----            '/'
--------------------------------------------------
divSimplification :: EvalFun -> EvalOp
divSimplification _ (BinOp _ OpMul lst) (CInteger constant)
    | any hasFraction lst = return . Left $ (binOp OpMul $ changeFraction lst)
        where hasFraction (Fraction _) = True
              hasFraction _ = False

              newCoeff frac = Fraction $ frac / toRational constant

              changeFraction [] = []
              changeFraction (Fraction f:xs) = newCoeff f : xs
              changeFraction (x:xs) = x : changeFraction xs

divSimplification _ a b = right (a,b)

--------------------------------------------------
----            cos
--------------------------------------------------
mod2piMulSimplify :: [FormulaPrim] -> FormulaPrim
mod2piMulSimplify lst
  | not $ any (NumEntity Pi ==) lst = binOp OpMul lst
  | otherwise = packFormula $ mapMaybe coeffReducer lst
      where packFormula [a] = a
            packFormula l = binOp OpMul l

            two :: Ratio Integer
            two = 2 % 1
            
            coeffReducer (CInteger n)
              | n `mod` 2 == 0 = Nothing
            coeffReducer (Fraction f)
              | f > two = coeffReducer . Fraction $ f - two
            coeffReducer a = Just a


{-piSignSimplify :: [FormulaPrim] -> FormulaPrim-}
{-piSignSimplify [Fraction f, NumEntity Pi]-}
    {-| f > 3 % 2 = KeepSign $ Fraction (2 % 1 - f) * NumEntity Pi-}
    {-| f > 1 % 1 = ChangeSign $ Fraction () * NumEntity Pi-}
    {-| f > 1 % 2 = ChangeSign $ Fraction (f - 1 % 2) * NumEntity Pi-}
{-piSignSimplify lst = KeepSign $ binOp OpMul lst-}

simplifyCos :: EvalFun -> FormulaPrim -> EqContext FormulaPrim
simplifyCos _eval (BinOp _ OpMul lst) = pure . cos $ mod2piMulSimplify lst
simplifyCos _ formula = pure $ cos formula

--------------------------------------------------
----            Sqrt
--------------------------------------------------
simplifySqrt :: EvalFun -> FormulaPrim -> EqContext FormulaPrim
simplifySqrt _eval (Fraction r)
  | isIntegerRoot (numerator r) && isIntegerRoot (denominator r) =
      return . Fraction $ integerRoot (numerator r) % integerRoot (denominator r)
  | isIntegerRoot (numerator r) =
      return $ (CInteger . integerRoot $ numerator r) 
             / (sqrt . CInteger $ denominator r)
  | isIntegerRoot (denominator r) = return $ (sqrt . CInteger $ numerator r) 
                                           / (CInteger . integerRoot $ denominator r)
     where integerRoot :: Integer -> Integer
           integerRoot i = let doubleValue = fromInteger i :: Double
                           in truncate $ sqrt doubleValue

           isIntegerRoot i = i == (integerRoot i) ^ (2 :: Int)
simplifySqrt _ formula = pure $ sqrt formula

--------------------------------------------------
----            Main Function
--------------------------------------------------
simplifyFormula :: EvalFun -> FormulaPrim
                -> EqContext FormulaPrim
simplifyFormula f (BinOp _ OpAdd lst) =
    binEval OpAdd (addSimplification f) (addSimplification f) lst
simplifyFormula f (BinOp _ OpSub lst) =
    binEval OpSub (subSimplification f) (addSimplification f) lst
simplifyFormula f (BinOp _ OpMul lst) =
    binEval OpMul (mulSimplification f) (mulSimplification f) lst
simplifyFormula f (BinOp _ OpDiv lst) =
    binEval OpDiv (divSimplification f) (mulSimplification f) lst
simplifyFormula f (UnOp _ OpSqrt sub) = simplifySqrt f sub
simplifyFormula f (UnOp _ OpCos sub) = simplifyCos f sub
simplifyFormula _ formu = pure formu

