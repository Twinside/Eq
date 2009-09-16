module EqManips.Renderer.Latex ( latexRender, latexRenderS ) where

import EqManips.Types
import EqManips.Algorithm.Utils
import EqManips.Propreties

latexRender :: Formula -> String
latexRender f = latexRenderS f ""

latexRenderS :: Formula -> ShowS
latexRenderS f = (str "\\begin{equation}\n")
              . lno (treeIfyFormula f) 
              . (str "\n\\end{equation}\n")

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

lno :: Formula -> ShowS
lno = l (Nothing, False)

l :: (Maybe BinOperator, Bool) -> Formula -> ShowS
l _ (Block _ _ _) = str "block"
l _ (Variable v) = str v
l _ (NumEntity e) = str $ latexOfEntity e
l _ (Truth t) = shows t
l _ (CInteger i) = shows i
l _ (CFloat d) = shows d
l op (Meta _ f) = l op f
l _ (Lambda _clauses) = id

l _ (BinOp OpDiv [a,b]) = str "\\frac{" . lno a . str "}{" . lno b . char '}'
l _ (BinOp OpPow [a,b]) = char '{' . l (Just OpPow, False) a 
                                   . str "}^{" . l (Just OpPow, True) b . char '}'
l (Just pop,right) (BinOp op [a,b])
    | needParenthesis right pop op =
        str "\\left( " . l (Just op, False) a 
                       . str (stringOfBinOp op) 
                       . l (Just op, True) b . str "\\right) "
    | otherwise = l (Just op, False) a . str (stringOfBinOp op) . l (Just op, True) b
l (Nothing,_) (BinOp op [a,b]) = lno a . str (stringOfBinOp op) . lno b
l _ (BinOp _ _) = error "latexification require treeified formula"

-- Unary operators
l _ (UnOp OpAbs f) = str "\\lvert " . lno f . str "\\rvert "
l _ (UnOp OpFloor f) = str "\\lfloor " . lno f . str "\\rfloor"
l _ (UnOp OpCeil f) = str "\\lceil " . lno f . str "\\rceil"
l _ (UnOp OpFrac f) = str "\\lbrace " . lno f . str "\\rbrace"
l _ (UnOp OpSqrt f) = str "\\sqrt{" . lno f . char '}'
l _ (UnOp OpExp f) = str "\\exp ^ {" . l (Just OpPow, True) f . str "} "
l _ (UnOp OpNegate f) 
    | f `hasProp` LeafNode = str " -" . lno f
    | otherwise = str "-\\left( " . lno f . str "\\right)"
l _ (UnOp OpFactorial f) 
    | f `hasProp` LeafNode = lno f . str "!"
    | otherwise = str "\\left( " . lno f . str "\\right)!"
l _ (UnOp op f)
    | f `hasProp` LeafNode = str (stringOfUnOp op) . lno f
    | otherwise = str (stringOfUnOp op) . str "\\left(" . lno f . str "\\right)"

l _ (Sum begin end what) =
    str "\\sum_{" . lno begin . str "}^{" . lno end . str "} " . lno what
l _ (Product begin end what) =
    str "\\prod_{" . lno begin . str "}^{" . lno end . str "} " . lno what

l _ (Integrate begin end what var) =
    str "\\int_{" . lno begin . str "}^{" . lno end 
                  . str "} \\! " . lno what . str " \\, d" . lno var

l _ (Derivate f var) =
    str "\\frac{d " . lno f . str "}{ d" . lno var . char '}'

l _ (App func args) = 
    lno func . str "\\left(" . latexargs args . str "\\right)"
     where latexargs [] = id
           latexargs (x:xs) = foldr (\e acc -> lno e . str ", " . acc)
                                    (lno x) xs

l _ (Matrix _ _ lsts) = str "\\begin{bmatrix}\n"
                      . matrixCells
                      . str "\\end{bmatrix}\n"
    where perLine lst = interspereseS (str " & ") $ map lno lst
          matrixCells = interspereseS (str "\\\\\n") $ map perLine lsts


