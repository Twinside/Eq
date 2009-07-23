module EqManips.Algorithm.Eval where

import EqManips.Types
import EqManips.Algorithm.Cleanup
import EqManips.Algorithm.Derivative

data ValType =
      CInt of Integer
    | CFl of Float
    | CMatrix Int Int [[ValType]]
    deriving (Eq, Show)

{-fromInteger-}

eval (Variable String) = 
eval (NumEntity Entity)
eval (CInteger i) = return . CInt $ toInteger i
eval (CFloat Double) = return $CFl d

eval (UnOp UnOperator Formula)
eval (BinOp BinOperator Formula Formula)

eval (Matrix n m lines) = do
    cells <- sequence [mapM eval mline | mline <- lines]
    return $ CMatrix n m cells

eval (App Formula [Formula])
eval (Sum Formula Formula Formula)
eval (Product Formula Formula Formula)
eval (Derivate Formula Formula)
eval (Integrate Formula Formula Formula Formula)
eval f@(Block _ _ _) =
    eqFail f "Block cannot be evaluated"
