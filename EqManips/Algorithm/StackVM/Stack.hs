{-# OPTIONS_GHC -fno-warn-orphans #-}
module EqManips.Algorithm.StackVM.Stack where

import Control.Applicative
import EqManips.Types

-- TODO : remove this two instances and/or
-- put them somewhere else
instance Applicative (Either a) where
    pure = Right
    (<*>) (Left a) _ = Left a
    (<*>) (Right _) (Left b) = Left b
    (<*>) (Right f) (Right v) = Right (f v)

instance Monad (Either a) where
    return = Right
    (>>=) (Left a) _ = Left a
    (>>=) (Right v) f = f v

type ValueType = Double

data StackOperand =
      Add | Sub | Mul | Div
    | Pow | Negate | Abs | Sqrt
    | Sin | Sinh | ASin | ASinh
    | Cos | Cosh | ACos | ACosh
    | Tan | Tanh | ATan | ATanh
    | Ln | Log | Exp
    | Factorial
    | Ceil | Floor | Frac
    | LoadX
    | LoadY
    | LoadConst ValueType

type CompiledExpression = [StackOperand]

type MachineWorld = [ValueType]

-- | bla
evalProgram :: CompiledExpression -> ValueType -> ValueType
            -> ValueType
evalProgram _program _x _y = 0.0

evalOperation :: ValueType -> ValueType -> MachineWorld
              -> MachineWorld
evalOperation _x _y _stack = []

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
stackOpOfUnop OpFactorial = Factorial
stackOpOfUnop OpCeil = Ceil 
stackOpOfUnop OpFloor = Floor 
stackOpOfUnop OpFrac = Frac

compileExpression :: FormulaPrim -> Either String CompiledExpression
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
compileExpression (UnOp _ op sub) =
    (++ [stackOpOfUnop op]) <$> compileExpression sub

compileExpression (BinOp _ op formulas) =
  case stackOpOfBinop op of
    Just stackOp -> foldr (\lst acc -> lst ++ (stackOp : acc)) []
                 <$> mapM compileExpression formulas
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

