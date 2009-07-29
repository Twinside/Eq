module EqManips.Linker( linkFormula ) where

import EqManips.Types
import Data.List

linkFormula :: Formula -> Formula
linkFormula = link

link :: Formula -> Formula
link (Variable "pi") = NumEntity Pi
-- Special cases
link (App (Variable "block") [CInteger i1, CInteger i2, CInteger i3]) = 
    Block i1 i2 i3
link (App (Variable "abs") [x]) = UnOp OpAbs $ link x
link (App (Variable "sqrt") [x]) = UnOp OpSqrt $ link x
link (App (Variable "exp") [x]) = UnOp OpExp $ link x
link (App (Variable "sum") [ini, end, what]) = 
    Sum (link ini) (link end) (link what)
link (App (Variable "sum") [ini, what]) = 
    Sum (link ini) (Variable "") (link what)
link (App (Variable "sum") [what]) = 
    Sum (Variable "") (Variable "") (link what)

link (App (Variable "sin") [x]) = UnOp OpSin $ link x
link (App (Variable "sinh") [x]) = UnOp OpSinh $ link x
link (App (Variable "asin") [x]) = UnOp OpASin $ link x
link (App (Variable "asinh") [x]) = UnOp OpASinh $ link x

link (App (Variable "cos") [x]) = UnOp OpCos $ link x
link (App (Variable "cosh") [x]) = UnOp OpCosh $ link x
link (App (Variable "acos") [x]) = UnOp OpACos $ link x
link (App (Variable "acosh") [x]) = UnOp OpACosh $ link x

link (App (Variable "tan") [x]) = UnOp OpTan $ link x
link (App (Variable "tanh") [x]) = UnOp OpTanh $ link x
link (App (Variable "atan") [x]) = UnOp OpATan $ link x
link (App (Variable "atanh") [x]) = UnOp OpATanh $ link x

link (App (Variable "ln") [x]) = UnOp OpLn $ link x
link (App (Variable "log") [x]) = UnOp OpLog $ link x

link (App (Variable "derivate") [what, var]) =
    Derivate (link what) (link var)

link (App (Variable "product") [ini, end, what]) = 
    Product (link ini) (link end) (link what)
link (App (Variable "product") [ini, what]) = 
    Product (link ini) (Variable "") (link what)
link (App (Variable "product") [what]) = 
    Product (Variable "") (Variable "") (link what)
    
link (App (Variable "integrate") [ini, end, what, dvar]) = 
    Integrate (link ini) (link end) (link what) (link dvar)
link (App (Variable "integrate") [ini, what, dvar]) = 
    Integrate (link ini) (Variable "") (link what) (link dvar)
link (App (Variable "integrate") [what, dvar]) = 
    Integrate (Variable "") (Variable "") (link what) (link dvar)

link (App (Variable "matrix") (CInteger n: CInteger m: exps))
    | n * m > length exps = error "The matrix has not enough expressions"
    | n * m < length exps = error "The matrix has too much expressions"
    | otherwise = Matrix n m $ splitMatrix exps
        where splitMatrix  [] = []
              splitMatrix lst =
                let (matrixLine, matrixRest) = genericSplitAt n lst
                in matrixLine : splitMatrix matrixRest

-- General transformations
link (App f flst) = App (link f) $ map link flst
link (UnOp op f) = UnOp op $ link f
link (BinOp op f1 f2) = BinOp op (link f1) (link f2)
link x = x

