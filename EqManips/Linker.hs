-- | This module will link variable names to
-- symbols.
module EqManips.Linker( linkFormula ) where

import EqManips.Types
import Data.List

linkFormula :: Formula -> Formula
linkFormula = link

-- | Function associating variables to symbol.
-- It's a crude way to do it... 
-- may need to change it to a real symbol table later
link :: Formula -> Formula
link (Variable "infinite") = NumEntity Infinite
link (Variable "pi") = NumEntity Pi
-- Meta cases
link (App (Variable "Hold") [f]) = Meta Hold $ link f
link (App (Variable "Force") [f]) = Meta Force $ link f
link (App (Variable "Listify") [f]) = Meta Listify $ link f
link (App (Variable "Treefy") [f]) = Meta Treefy $ link f
link (App (Variable "Expand") [f]) = Meta Expand $ link f
link (App (Variable "Cleanup") [f]) = Meta Cleanup $ link f
link (App (Variable "Sort") [f]) = Meta Sort $ link f
link (App (Variable "Lambda") [arg, body]) = Meta LambdaBuild $ Lambda [([arg], body)]

-- Special cases
link (App (Variable "block") [CInteger i1, CInteger i2, CInteger i3]) = 
    Block (fromEnum i1) (fromEnum i2) (fromEnum i3)
link (App (Variable "abs") [x]) = UnOp OpAbs $ link x
link (App (Variable "sqrt") [x]) = UnOp OpSqrt $ link x
link (App (Variable "exp") [x]) = UnOp OpExp $ link x
link (App (Variable "sum") [ini, end, what]) = 
    Sum (link ini) (link end) (link what)
link (App (Variable "sum") [ini, what]) = 
    Sum (link ini) (Variable "") (link what)
link (App (Variable "sum") [what]) = 
    Sum (Variable "") (Variable "") (link what)

link (App (Variable "ceil") [x]) = UnOp OpCeil $ link x
link (App (Variable "floor") [x]) = UnOp OpFloor $ link x
link (App (Variable "frac") [x]) = UnOp OpFrac $ link x

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
    | fromEnum n * fromEnum m > length exps = error "The matrix has not enough expressions"
    | fromEnum n * fromEnum m < length exps = error "The matrix has too much expressions"
    | otherwise = Matrix (fromEnum n) (fromEnum m) $ splitMatrix exps
        where splitMatrix  [] = []
              splitMatrix lst =
                let (matrixLine, matrixRest) = genericSplitAt n lst
                in map link matrixLine : splitMatrix matrixRest

-- General transformations
link (App f flst) = App (link f) $ map link flst
link (UnOp op f) = UnOp op $ link f
link (BinOp op fs) = BinOp op [link f | f <- fs]
link (Meta m fs) = Meta m $ link fs
link a@(CFloat _) = a
link a@(CInteger _) = a
link a@(NumEntity _) = a
link a@(Block _ _ _) = a
link v@(Variable _) = v
link t@(Truth _) = t
link (Lambda l) = Lambda [ (map link fl, link f)| (fl, f) <- l]
link (Matrix n m ll) = Matrix n m [map link rows | rows <- ll]
link (Derivate a b) = Derivate (link a) (link b)
link (Sum a b c) = Sum (link a) (link b) (link c)
link (Product a b c) = Sum (link a) (link b) (link c)
link (Integrate a b c d) = Integrate (link a) (link b) (link c) (link d)

