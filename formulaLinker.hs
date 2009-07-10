module FormulaLinker( linkFormula ) where

import FormulaTypes

linkFormula :: Formula -> Formula
linkFormula = link

link :: Formula -> Formula
-- Special cases
link (App (Variable "abs") [x]) = UnOp OpAbs $ link x
link (App (Variable "sqrt") [x]) = UnOp OpSqrt $ link x
link (App (Variable "sum") [ini, end, what]) = 
    Sum (link ini) (link end) (link what)
link (App (Variable "sum") [ini, what]) = 
    Sum (link ini) (Variable "") (link what)
link (App (Variable "sum") [what]) = 
    Sum (Variable "") (Variable "") (link what)

-- General transformations
link (App f flst) = App (link f) $ map link flst
link (UnOp op f) = UnOp op $ link f
link (BinOp op f1 f2) = BinOp op (link f1) (link f2)
link x = x

