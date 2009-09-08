module EqManips.Renderer.Latex ( latexRender ) where

import EqManips.Types
import EqManips.Algorithm.Utils
import EqManips.Propreties

latexRender :: Formula -> String
latexRender f = l (treeIfyFormula f) ""

str :: String -> ShowS
str = (++)

char :: Char -> ShowS
char = (:)

latexOfEntity :: Entity -> String
latexOfEntity Pi = "\\pi"
latexOfEntity Nabla = "\\nabla"
latexOfEntity Infinite = "\\infty"

stringOfUnOp :: UnOperator -> String
stringOfUnOp OpSin = "\\sin"
stringOfUnOp OpSinh  = "\\sinh"
stringOfUnOp OpASin  = "\\arcsin"
stringOfUnOp OpASinh = "\\arcsinh"
stringOfUnOp OpCos  = "\\cos"
stringOfUnOp OpCosh  = "\\cosh"
stringOfUnOp OpACos  = "\\arccos"
stringOfUnOp OpACosh = "\\arccosh"
stringOfUnOp OpTan  = "\\tan"
stringOfUnOp OpTanh  = "\\tanh"
stringOfUnOp OpATan  = "\\arctan"
stringOfUnOp OpATanh = "\\arctanh"
stringOfUnOp OpLn = "\\ln"
stringOfUnOp OpLog = "\\log"
stringOfUnOp op = error $ "stringOfUnop : unknown op " ++ show op

stringOfBinOp :: BinOperator -> String
stringOfBinOp OpAdd = "+"
stringOfBinOp OpSub = "-"
stringOfBinOp OpMul = "\\ast"
stringOfBinOp OpDiv = "\\div"
stringOfBinOp OpAnd = " \\and "
stringOfBinOp OpOr = " \\or "
stringOfBinOp OpEq = " = "
stringOfBinOp OpNe = " \\ne "
stringOfBinOp OpLt = " < "
stringOfBinOp OpGt = " > "
stringOfBinOp OpGe = " \\ge "
stringOfBinOp OpLe = " \\le "
stringOfBinOp _ = error "stringOfBinOp - unknown op"

l :: Formula -> ShowS
l (Variable v) = str v
l (NumEntity e) = str $ latexOfEntity e
l (Truth t) = shows t
l (CInteger i) = shows i
l (CFloat d) = shows d
l (Meta _ f) = l f
l (Lambda _clauses) = id

l (BinOp OpDiv [a,b]) = str "\\frac{" . l a . str "}{" . l b . char '}'
l (BinOp OpPow [a,b]) = char '{' . l a . str "}^{" . l b . char '}'
l (BinOp op [a,b]) = l a . str (stringOfBinOp op) . l b
l (BinOp _ _) = error "latexification require treeified formula"

-- Unary operators
l (UnOp OpNegate f) = str " -" . l f
l (UnOp OpFactorial f) = l f . str "!"
l (UnOp OpAbs f) = str "\\lvert " . l f . str "\\rvert "
l (UnOp OpFloor f) = str "\\lfloor " . l f . str "\\rfloor"
l (UnOp OpCeil f) = str "\\lceil " . l f . str "\\rceil"
l (UnOp OpFrac f) = str "\\lbrace " . l f . str "\\rbrace"
l (UnOp OpSqrt f) = str "\\sqrt{" . l f . char '}'
l (UnOp OpExp f) = str "\\exp ^ {" . l f . str "} "
l (UnOp op f)
    | f `hasProp` LeafNode = str (stringOfUnOp op) . l f
    | otherwise = str (stringOfUnOp op) . str "\\left(" . l f . str "\\right)"

l (Sum begin end what) =
    str "\\sum_{" . l begin . str "}^{" . l end . str "} " . l what
l (Product begin end what) =
    str "\\prod_{" . l begin . str "}^{" . l end . str "} " . l what

l (Integrate begin end what var) =
    str "\\int_{" . l begin . str "}^{" . l end 
                  . str "} \\! " . l what . str " \\, d" . l var

l (Derivate f var) =
    str "\\frac{d " . l f . str "}{ d" . l var . str "}"

l (App func args) = 
    l func . str "\\left(" . latexargs . str "\\right)"
     where latexargs = id

l (Matrix _ _ lsts) = str "\\begin{bmatrix}\n"
                    . matrixCells
                    . str "\\end{bmatrix}\n"
    where writeLine curr acc = l curr . str " & " . acc
          liner = foldr writeLine (str "\\\\\n")
          matrixCells = foldr (\mline acc -> liner mline . acc) id lsts

