{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Eq.Algorithm.StackVM.Stack( compileExpression
                                       , evalProgram 
                                       , ValueType
                                       ) where

import Control.Applicative
import Data.List( foldl' )

import Language.Eq.Types
import Language.Eq.Polynome
import Language.Eq.Algorithm.Cleanup( cleanupFormulaPrim )

type ValueType = Double

data StackOperand =
      Add | Sub | Mul | Div
    | Pow | Negate | Abs | Sqrt
    | Sin | Sinh | ASin | ASinh
    | Cos | Cosh | ACos | ACosh
    | Tan | Tanh | ATan | ATanh
    | Ln | Log | Exp
    | Ceil | Floor | Frac
    | LoadX
    | LoadY
    | LoadConst ValueType
    deriving Show

type CompiledExpression = [StackOperand]

type MachineWorld = [ValueType]

-- | bla
evalProgram :: CompiledExpression -> ValueType -> ValueType
            -> ValueType
evalProgram program x y = head $ foldl' (evalOperation x y) [] program

-- | Main eval function.
evalOperation :: ValueType -> ValueType -> MachineWorld
              -> StackOperand
              -> MachineWorld
evalOperation _ _ rest (LoadConst v) = v : rest
evalOperation x _ rest LoadX = x : rest
evalOperation _ y rest LoadY = y : rest

evalOperation _ _ (v1:v2:rest) Add = (v2 + v1) : rest
evalOperation _ _ (v1:v2:rest) Sub = (v2 - v1) : rest
evalOperation _ _ (v1:v2:rest) Mul = (v2 * v1) : rest
evalOperation _ _ (v1:v2:rest) Div = (v2 / v1) : rest
evalOperation _ _ (v1:v2:rest) Pow = (v2 ** v1) : rest

evalOperation _ _ (v1:rest) Negate = (-v1) : rest
evalOperation _ _ (v1:rest) Abs = (-v1) : rest
evalOperation _ _ (v1:rest) Sqrt = sqrt v1 : rest
evalOperation _ _ (v1:rest) Sin  = sin  v1 : rest
evalOperation _ _ (v1:rest) Sinh  = sinh  v1 : rest
evalOperation _ _ (v1:rest) ASin  = asin  v1 : rest
evalOperation _ _ (v1:rest) ASinh = asinh v1 : rest
evalOperation _ _ (v1:rest) Cos  = cos  v1 : rest
evalOperation _ _ (v1:rest) Cosh  = cosh  v1 : rest
evalOperation _ _ (v1:rest) ACos  = acos  v1 : rest
evalOperation _ _ (v1:rest) ACosh = acosh v1 : rest
evalOperation _ _ (v1:rest) Tan  = tan  v1 : rest
evalOperation _ _ (v1:rest) Tanh  = tanh  v1 : rest
evalOperation _ _ (v1:rest) ATan  = atan  v1 : rest
evalOperation _ _ (v1:rest) ATanh = atanh v1 : rest
evalOperation _ _ (v1:rest) Ln  = log  v1 : rest
evalOperation _ _ (v1:rest) Log  = (log v1 / log 10) : rest
evalOperation _ _ (v1:rest) Exp = exp v1 : rest

evalOperation _ _ (v1:rest) Ceil = (fromInteger $ ceiling v1) : rest
evalOperation _ _ (v1:rest) Floor = (fromInteger $ floor v1) : rest
evalOperation _ _ (v1:rest) Frac = v' : rest
    where (_, v') = properFraction v1 :: (Int,Double)

evalOperation _ _ [] _ = error "Stack VM : empty stack."
evalOperation _ _ _ _ = error "Stack VM : stack underflow"


stackOpOfBinop :: BinOperator -> Maybe StackOperand
stackOpOfBinop OpAdd = Just Add  
stackOpOfBinop OpSub = Just Sub 
stackOpOfBinop OpMul = Just Mul 
stackOpOfBinop OpDiv = Just Div 
stackOpOfBinop OpPow = Just Pow 
stackOpOfBinop _ = Nothing

stackOpOfUnop :: UnOperator -> StackOperand
stackOpOfUnop OpNegate = Negate 
stackOpOfUnop OpAbs = Abs 
stackOpOfUnop OpSqrt = Sqrt
stackOpOfUnop OpSin = Sin 
stackOpOfUnop OpSinh = Sinh 
stackOpOfUnop OpASin = ASin 
stackOpOfUnop OpASinh = ASinh
stackOpOfUnop OpCos = Cos 
stackOpOfUnop OpCosh = Cosh 
stackOpOfUnop OpACos = ACos 
stackOpOfUnop OpACosh = ACosh
stackOpOfUnop OpTan = Tan 
stackOpOfUnop OpTanh = Tanh 
stackOpOfUnop OpATan = ATan 
stackOpOfUnop OpATanh = ATanh
stackOpOfUnop OpLn = Ln 
stackOpOfUnop OpLog = Log 
stackOpOfUnop OpExp = Exp
stackOpOfUnop OpFactorial =
    error "Cannot be compiled"
stackOpOfUnop OpCeil = Ceil 
stackOpOfUnop OpFloor = Floor 
stackOpOfUnop OpFrac = Frac

-- | Convert a polynome into a formula to provide the minimal
-- formula in term of multiplication need.
convertPolynomeToEvalFormula :: Polynome -> Maybe FormulaPrim
convertPolynomeToEvalFormula (PolyRest c) = Just $ coefToFormula c
convertPolynomeToEvalFormula (Polynome [var] polyCoeffs) 
    | var == 'x' || var == 'y' = do
      firstTransfo <- convertPolynomeToEvalFormula firstSub
      (lastCoeff, lastFormu) <-
                 foldl' prefCoeff (Just (firstCoeff, firstTransfo)) restCoeff
      pure . cleanupFormulaPrim $ lastFormu * fvar ** coefToFormula lastCoeff
        where ((firstCoeff,firstSub):restCoeff) = reverse polyCoeffs
              fvar = Variable [var]

              multCoeff :: FormulaPrim -> PolyCoeff -> PolyCoeff -> FormulaPrim
                        -> (PolyCoeff, FormulaPrim)
              multCoeff rez _             0 subFormu = (0        , rez + subFormu)
              multCoeff rez 0         coeff subFormu = (coeff - 1, rez * fcoeff * fvar * subFormu)
                where fcoeff = coefToFormula coeff
              multCoeff rez prevCoeff coeff subFormu =
                  (coeff, (rez * fvar ** thisCoeff + 1) * subFormu)
                where thisCoeff = coefToFormula $ prevCoeff - coeff

              prefCoeff :: Maybe (PolyCoeff, FormulaPrim) -> (PolyCoeff, Polynome)
                        -> Maybe (PolyCoeff, FormulaPrim)
              prefCoeff Nothing                            _ = Nothing
              prefCoeff (Just (prevCoeff, rez)) (coeff, sub) = do
                  multCoeff rez prevCoeff coeff <$> convertPolynomeToEvalFormula sub
              

convertPolynomeToEvalFormula (Polynome _ _) = Nothing

compileExpression :: FormulaPrim -> Either String CompiledExpression
compileExpression (Poly _ p) =
    maybe (Left "Wrong variable name in expression") compileExpression
          $ convertPolynomeToEvalFormula p

compileExpression (Variable "x") = Right [LoadX]
compileExpression (Variable "y") = Right [LoadY]
compileExpression (NumEntity Pi) = Right [LoadConst pi]
compileExpression (NumEntity _) = 
    Left "Can't compile numeric entity"
compileExpression (Variable v) =
    Left $ "Can't compile expression with unbound variable ("
                ++ v ++ ")"
compileExpression (CInteger i) = Right [LoadConst $ fromInteger i]
compileExpression (CFloat f) = Right [LoadConst f]
compileExpression (Fraction f) = Right [LoadConst $ fromRational f]
compileExpression (UnOp _ OpFactorial _) =
    Left "Cannot compile factorial expression"
compileExpression (UnOp _ op sub) =
    (++ [stackOpOfUnop op]) <$> compileExpression sub

compileExpression (BinOp _ op formulas) =
  case stackOpOfBinop op of
    Just stackOp -> case mapM compileExpression formulas of
        Left err -> Left err
        Right [] -> Left "Stack VM : Empty binop"
        Right [x] -> Right x
        Right (x:xs) ->
            Right $ x ++ foldr (\lst acc -> lst ++ (stackOp : acc)) [] xs
    Nothing -> Left "Error non continuous operators used"
compileExpression (App _ _ _) =
    Left "No function call allowed in compiled expression."
compileExpression (Sum _ _ _ _) =
    Left "No sum allowed."
compileExpression (Product _ _ _ _) =
    Left "No product allowed."
compileExpression (Indexes _ _ _) =
    Left "No indexes allowed in compiled exprression."
compileExpression (List _ _) =
    Left "No lists allowed in compiled exprression."
compileExpression (Complex _ _) =
    Left "No complex arithmetic allowed in compiled expression."
compileExpression (Lambda _ _) = 
    Left "No lambda allowed in compiled expression."
compileExpression (Matrix _ _ _ _) = 
    Left "No matrix allowed in compiled expression."
compileExpression (Truth _) = 
    Left "No boolean expression allowed for compilation."
compileExpression (Derivate _ _ _) = 
    Left "No derivation allowed in compilation."
compileExpression (Integrate _ _ _ _ _) = 
    Left "No integration allowed in compilation."
compileExpression (Block _ _ _) = 
    Left "There is some errors in expressions."
compileExpression (Meta _ _ _) =
    Left "No meta operations allowed in compilation."

