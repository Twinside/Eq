module EqManips.Renderer.Latex ( latexRender, latexRenderS ) where

import Data.Ratio

import EqManips.Types
import EqManips.Polynome
import EqManips.Algorithm.Utils
import EqManips.Propreties

import EqManips.Renderer.RenderConf

latexRender :: Conf -> Formula TreeForm -> String
latexRender conf f = latexRenderS conf f ""

latexRenderS :: Conf -> Formula TreeForm -> ShowS
latexRenderS conf(Formula f) = (str "\\begin{equation}\n")
                             . lno conf f 
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
stringOfBinOp OpAttrib = " := "
stringOfBinOp _ = error "stringOfBinOp - unknown op"

lno :: Conf -> FormulaPrim -> ShowS
lno conf = l conf (Nothing, False)

l :: Conf -> (Maybe BinOperator, Bool) -> FormulaPrim -> ShowS
l conf op (Poly p) = l conf op . unTagFormula . treeIfyFormula $ convertToFormula p
l conf op (Fraction f) = l conf op $ (CInteger $ numerator f) / (CInteger $ denominator f)
l conf op (Complex (real, complex)) = l conf op $ real + Variable "i" * complex
l _ _ (Block _ _ _) = str "block"
l _ _ (Variable v) = str v
l _ _ (NumEntity e) = str $ latexOfEntity e
l _ _ (Truth t) = shows t
l _ _ (CInteger i) = shows i
l _ _ (CFloat d) = shows d
l conf op (Meta _ f) = l conf op f
l _ _ (Lambda _clauses) = id

l conf (Just pop,right) (BinOp OpMul [a,b])
    | mulAsDot conf = if needParenthesis right pop OpMul
            then str "\\left( " . expr . str "\\right) "
            else expr
        where expr = l conf (Just OpMul, False) a
                   . str "\\cdot"
                   . l conf (Just OpMul, True) b

l conf (Nothing,_) (BinOp OpMul [a,b])
    | mulAsDot conf =
        l conf (Just OpMul, False) a . str "\\cdot" . l conf (Just OpMul, True) b

l conf _ (BinOp OpDiv [a,b]) = str "\\frac{" . lno conf a . str "}{" . lno conf b . char '}'
l conf _ (BinOp OpPow [a,b]) = char '{' . l conf (Just OpPow, False) a 
                                   . str "}^{" . l conf (Just OpPow, True) b . char '}'
l conf (Just pop,right) (BinOp op [a,b]) =
    if needParenthesis right pop op
        then str "\\left( " . expr . str "\\right) "
        else expr
      where expr = l conf (Just op, False) a 
                 . str (stringOfBinOp op) 
                 . l conf (Just op, True) b

l conf (Nothing,_) (BinOp op [a,b]) = lno conf a . str (stringOfBinOp op) . lno conf b
l _ _ (BinOp _ _) = error "latexification require treeified formula"

-- Unary operators
l conf _ (UnOp OpAbs f) = str "\\lvert " . lno conf f . str "\\rvert "
l conf _ (UnOp OpFloor f) = str "\\lfloor " . lno conf f . str "\\rfloor"
l conf _ (UnOp OpCeil f) = str "\\lceil " . lno conf f . str "\\rceil"
l conf _ (UnOp OpFrac f) = str "\\lbrace " . lno conf f . str "\\rbrace"
l conf _ (UnOp OpSqrt f) = str "\\sqrt{" . lno conf f . char '}'
l conf _ (UnOp OpExp f) = str "\\exp ^ {" . l conf (Just OpPow, True) f . str "} "
l conf _ (UnOp OpNegate f) 
    | f `hasProp` LeafNode = str " -" . lno conf f
    | otherwise = str "-\\left( " . lno conf f . str "\\right)"
l conf _ (UnOp OpFactorial f) 
    | f `hasProp` LeafNode = lno conf f . str "!"
    | otherwise = str "\\left( " . lno conf f . str "\\right)!"
l conf _ (UnOp op f)
    | f `hasProp` LeafNode = str (stringOfUnOp op) . lno conf f
    | otherwise = str (stringOfUnOp op) . str "\\left(" . lno conf f . str "\\right)"

l conf _ (Sum begin end what) =
    str "\\sum_{" . lno conf begin . str "}^{" . lno conf end . str "} " . lno conf what
l conf _ (Product begin end what) =
    str "\\prod_{" . lno conf begin . str "}^{" . lno conf end . str "} " . lno conf what

l conf _ (Integrate begin end what var) =
    str "\\int_{" . lno conf begin . str "}^{" . lno conf end 
                  . str "} \\! " . lno conf what . str " \\, d" . lno conf var

l conf _ (Derivate f var) =
    str "\\frac{d " . lno conf f . str "}{ d" . lno conf var . char '}'

l conf _ (App func args) = 
    lno conf func . str "\\left(" . latexargs args . str "\\right)"
     where latexargs [] = id
           latexargs (x:xs) = foldr (\e acc -> lno conf e . str ", " . acc)
                                    (lno conf x) xs

l conf _ (Matrix _ _ lsts) = str "\\begin{bmatrix}\n"
                      . matrixCells
                      . str "\\end{bmatrix}\n"
    where perLine lst = interspereseS (str " & ") $ map (lno conf) lst
          matrixCells = interspereseS (str "\\\\\n") $ map perLine lsts


