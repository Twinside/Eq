module EqManips.Linker( linkFormula ) where

import EqManips.Types

linkFormula :: Formula -> Formula
linkFormula = link

link :: Formula -> Formula
-- Special cases
link (App (Variable "block") [CInteger i1, CInteger i2, CInteger i3]) = 
    Block i1 i2 i3
link (App (Variable "abs") [x]) = UnOp OpAbs $ link x
link (App (Variable "sqrt") [x]) = UnOp OpSqrt $ link x
link (App (Variable "sum") [ini, end, what]) = 
    Sum (link ini) (link end) (link what)
link (App (Variable "sum") [ini, what]) = 
    Sum (link ini) (Variable "") (link what)
link (App (Variable "sum") [what]) = 
    Sum (Variable "") (Variable "") (link what)

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

-- General transformations
link (App f flst) = App (link f) $ map link flst
link (UnOp op f) = UnOp op $ link f
link (BinOp op f1 f2) = BinOp op (link f1) (link f2)
link x = x

