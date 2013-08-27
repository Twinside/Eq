module Language.Eq.Renderer.Latex ( latexRender, latexRenderS ) where

import Data.Ratio

import Language.Eq.Types
import Language.Eq.Polynome
import Language.Eq.Algorithm.Utils
import Language.Eq.Propreties

import Language.Eq.Renderer.RenderConf

latexRender :: Conf -> Formula TreeForm -> String
latexRender conf f = latexRenderS conf f ""

latexRenderS :: Conf -> Formula TreeForm -> ShowS
latexRenderS conf(Formula f) = str "\\begin{equation*}\n"
                             . lno conf f 
                             . str "\n\\end{equation*}\n"

str :: String -> ShowS
str = (++)

char :: Char -> ShowS
char = (:)

latexOfEntity :: Entity -> String
latexOfEntity Pi = "\\pi "
latexOfEntity Nabla = "\\nabla "
latexOfEntity Infinite = "\\infty "
latexOfEntity Ellipsis = "\\cdots"

stringOfUnOp :: UnOperator -> String
stringOfUnOp OpSin = "\\sin "
stringOfUnOp OpSinh  = "\\sinh "
stringOfUnOp OpASin  = "\\arcsin "
stringOfUnOp OpASinh = "\\arcsinh "
stringOfUnOp OpCos  = "\\cos "
stringOfUnOp OpCosh  = "\\cosh "
stringOfUnOp OpACos  = "\\arccos "
stringOfUnOp OpACosh = "\\arccosh "
stringOfUnOp OpTan  = "\\tan "
stringOfUnOp OpTanh  = "\\tanh "
stringOfUnOp OpATan  = "\\arctan "
stringOfUnOp OpATanh = "\\arctanh "
stringOfUnOp OpLn = "\\ln "
stringOfUnOp OpLog = "\\log "
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

latexargs :: Conf -> [FormulaPrim] -> ShowS
latexargs _ [] = id
latexargs conf (x:xs) = foldr (\e acc -> lno conf e . str ", " . acc)
                              (lno conf x) xs

l :: Conf -> (Maybe BinOperator, Bool) -> FormulaPrim -> ShowS
l conf op (Poly _ p) = l conf op . unTagFormula . treeIfyFormula $ convertToFormula p
l conf op (Fraction f) = l conf op $ (CInteger $ numerator f) / (CInteger $ denominator f)
l conf op (Complex _ c) = l conf op $ complexTranslate c
l conf _ (List _ lst) = str "\\left[" . latexargs conf lst . str "\\right]"
l conf _ (Indexes _ main lst) = lno conf main . str "_{" . latexargs conf lst . char '}'
l _ _ (Block _ _ _) = str "block"
l _ _ (Variable v) = str v
l _ _ (NumEntity e) = str $ latexOfEntity e
l _ _ (Truth t) = shows t
l _ _ (CInteger i) = shows i
l _ _ (CFloat d) = shows d
l conf op (Meta _ _ f) = l conf op f
l _ _ (Lambda _ _clauses) = id

l conf (Just pop,right) (BinOp _ OpMul [a,b])
    | mulAsDot conf = if needParenthesis right pop OpMul
            then str "\\left( " . expr . str "\\right) "
            else expr
        where expr = l conf (Just OpMul, False) a
                   . str "\\cdot "
                   . l conf (Just OpMul, True) b

l conf (Nothing,_) (BinOp _ OpMul [a,b])
    | mulAsDot conf =
        l conf (Just OpMul, False) a . str "\\cdot " . l conf (Just OpMul, True) b

l conf _ (BinOp _ OpDiv [a,b]) = str "\\frac{" . lno conf a . str "}{" . lno conf b . char '}'
l conf _ (BinOp _ OpPow [a,b]) = char '{' . l conf (Just OpPow, False) a 
                                   . str "}^{" . l conf (Just OpPow, True) b . char '}'
l conf (Just pop,right) (BinOp _ op [a,b]) =
    if needParenthesis right pop op
        then str "\\left( " . expr . str "\\right) "
        else expr
      where expr = l conf (Just op, False) a 
                 . str (stringOfBinOp op) 
                 . l conf (Just op, True) b

l conf (Nothing,_) (BinOp _ op [a,b]) = lno conf a . str (stringOfBinOp op) . lno conf b
l _ _ (BinOp _ _ _) = error "latexification require treeified formula"
l _ _ (Infer _ _ _) = error "Can't export inference to LaTeX"

-- Unary operators
l conf _ (UnOp _ OpAbs f) = str "\\lvert " . lno conf f . str "\\rvert "
l conf _ (UnOp _ OpFloor f) = str "\\lfloor " . lno conf f . str "\\rfloor"
l conf _ (UnOp _ OpCeil f) = str "\\lceil " . lno conf f . str "\\rceil"
l conf _ (UnOp _ OpFrac f) = str "\\lbrace " . lno conf f . str "\\rbrace"
l conf _ (UnOp _ OpSqrt f) = str "\\sqrt{" . lno conf f . char '}'
l conf _ (UnOp _ OpExp f) = str "\\exp ^ {" . l conf (Just OpPow, True) f . str "} "
l conf _ (UnOp _ OpNegate f) 
    | f `hasProp` LeafNode = str " -" . lno conf f
    | otherwise = str "-\\left( " . lno conf f . str "\\right)"
l conf _ (UnOp _ OpFactorial f) 
    | f `hasProp` LeafNode = lno conf f . str "!"
    | otherwise = str "\\left( " . lno conf f . str "\\right)!"
l conf _ (UnOp _ op f)
    | f `hasProp` LeafNode = str (stringOfUnOp op) . lno conf f
    | otherwise = str (stringOfUnOp op) . str "\\left(" . lno conf f . str "\\right)"

l conf _ (Sum _ begin end what) =
    str "\\sum_{" . lno conf begin . str "}^{" . lno conf end . str "} " . lno conf what
l conf _ (Product _ begin end what) =
    str "\\prod_{" . lno conf begin . str "}^{" . lno conf end . str "} " . lno conf what

l conf _ (Integrate _ begin end what var) =
    str "\\int_{" . lno conf begin . str "}^{" . lno conf end 
                  . str "} \\! " . lno conf what . str " \\, d" . lno conf var

l conf _ (Derivate _ f var) =
    str "\\frac{d " . lno conf f . str "}{ d" . lno conf var . char '}'

l conf _ (App _ func args) = 
    lno conf func . str "\\left(" . latexargs conf args . str "\\right)"
     where 
l conf _ (Matrix _ _ _ lsts) = str "\\begin{bmatrix}\n"
                      . matrixCells
                      . str "\n\\end{bmatrix}"
    where perLine = interspereseS (str " & ") . map (lno conf)
          matrixCells = interspereseS (str "\\\\\n") $ map perLine lsts


