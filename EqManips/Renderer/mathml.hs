{-# LANGUAGE NewQualifiedOperators #-}
module EqManips.Renderer.Mathml( mathmlRender ) where

import EqManips.Types
import EqManips.Algorithm.Utils
import EqManips.Propreties

mathmlRender :: Formula -> String
mathmlRender f = (str "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n")
               . semantics ( presMarkup . annotation contentMarkup)
               . (str "</math>\n") $ ""
    where contentMarkup = content f
          presMarkup = prez f
          semantics = tagger "semantics"
          annotation c = str "<annotation-xml encoding=\"MathML-Content\">\n" 
                       . c . str "\n</annotation-xml>\n"

str :: String -> ShowS
str = (++)

char :: Char -> ShowS
char = (:)

mathMlOfEntity :: Entity -> String
mathMlOfEntity Pi = "<pi/>"
mathMlOfEntity Nabla = "<grad/>"
mathMlOfEntity Infinite = "<infinity/>"

tagger :: String -> ShowS -> ShowS
tagger tag f = str ('<': tag ++ ">") . f . str ("</" ++ tag ++ ">")

prez :: Formula -> ShowS
prez = presentation Nothing

cleanify :: String -> String
cleanify = concat . map deAnchor
    where deAnchor '<' = "&lt;"
          deAnchor '>' = "&gt;"
          deAnchor a = [a]

mo, mi, mn, mfrac, mrow, parens, msubsup, msqrt, mfenced, mtable, mtd :: ShowS -> ShowS
mo = tagger "mo"
mi = tagger "mi"
mn = tagger "mn"
mfrac = tagger "mfrac"
mrow = tagger "mrow"
parens f = str "<mo>(</mo>" . f . str "<mo>)</mo>"
msubsup = tagger "msubsup"
msqrt = tagger "msqrt"

mfenced f = str "<mfenced open=\"[\" close=\"]\">\n" . f . str "</mfenced>\n"
mtable = tagger "mtable"
mtd = tagger "mtd"

enclose :: Char -> Char -> ShowS -> ShowS
enclose beg end f = str ("<mo>" ++ (beg : "</mo>")) . f . str ("<mo>" ++ (end : "</mo>"))

presentation :: Maybe (BinOperator, Bool) -> Formula -> ShowS
presentation _ (Block _ _ _) = mi $ str "block"
presentation _ (Variable v) = mi $ str v
presentation _ (NumEntity e) = mn $ str $ mathMlOfEntity e
presentation _ (Truth t) = mn $ shows t
presentation _ (CInteger i) = mn $ shows i
presentation _ (CFloat d) = mn $ shows d
presentation inf (Meta _ f) = presentation inf f
presentation _ (Lambda _clauses) = id

presentation _ (BinOp OpDiv [a,b]) =
    mfrac $ mrow (prez a)
          . mrow (prez b)

presentation (Just (pop,isRight)) f@(BinOp op _)
    | needParenthesis isRight pop op = parens $ prez f

presentation Nothing (BinOp op [a,b]) =
    presentation (Just (op, False)) a
    . mo (str . cleanify $ binopString op)
    . presentation (Just (op, True)) b

-- Unary operators
presentation _ (UnOp OpFrac f) = enclose '{' '}' $ prez f
presentation _ (UnOp OpAbs f) = enclose '|' '|' $ prez f
presentation _ (UnOp OpSqrt f) = msqrt $ prez f
presentation _ (UnOp OpFactorial f)
  | f `hasProp` LeafNode = prez f . mo (char '!')
  | otherwise = (parens $ prez f) . mo (char '!')
presentation _ (UnOp OpNegate f)
  | f `hasProp` LeafNode = mo (char '-') . prez f
  | otherwise = mo (char '-') . (parens $ prez f)
presentation _ (UnOp op f)
  | f `hasProp` LeafNode = mo (str $ stringOfUnOp op) . prez f
  | otherwise = mo (str $ stringOfUnOp op) . parens (prez f)

presentation _ (Sum begin end what) =
    (msubsup $ mo (str "&sum;")
             . mrow (prez begin)
             . mrow (prez end)) . mrow (prez what)

presentation _ (Product begin end what) =
    (msubsup $ mo (str "&prod;")
             . mrow (prez begin)
             . mrow (prez end)) . mrow (prez what)

presentation _ (Integrate begin end what var) =
    (msubsup $ mo (str "&int;")
             . mrow (prez begin)
             . mrow (prez end)) . mrow (prez what . mi (str "d") . prez var)

presentation _ (Derivate f var) =
    (mfrac $ mi (char 'd')
           . mrow (mi (char 'd') . prez var)) . prez f

presentation _ (App func args) =
    prez func . parens (interspereseS (mo $ char ',') $ map prez args)

presentation _ (Matrix _ _ lsts) =
    mfenced $ mtable $ concatS [matrixrow $ concatS [ mtd $ prez cell | cell <- row] | row <- lsts ]
presentation _ _ = error "Wrong MathML presentation rendering"

-----------------------------------------------
----        Content
-----------------------------------------------

ci, cn, apply, lowlimit,
    uplimit, matrix, matrixrow,
    bvar :: ShowS -> ShowS

ci = tagger "ci"
cn = tagger "cn"
apply = tagger "apply"
lowlimit = tagger "lowlimit"
uplimit = tagger "uplimit"
matrix = tagger "matrix"
matrixrow = tagger "matrixrow"
bvar = tagger "bvar"

stringOfUnOp :: UnOperator -> String
stringOfUnOp OpSin = "<sin/>"
stringOfUnOp OpSinh  = "<sinh/>"
stringOfUnOp OpASin  = "<arcsin/>"
stringOfUnOp OpASinh = "<arcsinh/>"
stringOfUnOp OpCos  = "<cos/>"
stringOfUnOp OpCosh  = "<cosh/>"
stringOfUnOp OpACos  = "<arccos/>"
stringOfUnOp OpACosh = "<arccosh/>"
stringOfUnOp OpTan  = "<tan/>"
stringOfUnOp OpTanh  = "<tanh/>"
stringOfUnOp OpATan  = "<arctan/>"
stringOfUnOp OpATanh = "<arctanh/>"
stringOfUnOp OpLn = "<ln/>"
stringOfUnOp OpLog = "<log/>"
stringOfUnOp OpExp = "<exp/>"
stringOfUnOp OpAbs = "<abs/>"
stringOfUnOp OpFloor = "<floor/>"
stringOfUnOp OpCeil = "<ceiling/>"
stringOfUnOp OpSqrt = "<root/>"
stringOfUnOp OpFactorial = "<factorial/>"
stringOfUnOp op = error $ "stringOfUnop : unknown op " ++ show op

stringOfBinOp :: BinOperator -> String
stringOfBinOp OpAdd = "<plus/>"
stringOfBinOp OpAnd = "<and/>"
stringOfBinOp OpDiv = "<quotient/>"
stringOfBinOp OpEq = "<eq/>"
stringOfBinOp OpGe = "<geq/>"
stringOfBinOp OpGt = "<gt/>"
stringOfBinOp OpLe = "<leq/>"
stringOfBinOp OpLt = "<lt/>"
stringOfBinOp OpMul = "<times/>"
stringOfBinOp OpNe = "<neq/>"
stringOfBinOp OpOr = "<or/>"
stringOfBinOp OpPow = "<power/>"
stringOfBinOp OpSub = "<minus/>"

bigOperator :: [Char] -> String -> Formula -> Formula -> Formula
            -> ShowS
bigOperator operator var def end what = 
    apply $ str operator
          . bvar (str var)
          . lowlimit (content def)
          . uplimit (content end)
          . content what

-- | Give 2 xml trees, one for presentation and one
-- for content. Shitty MathML.
content :: Formula -> ShowS
content (Block _ _ _) = ci $ str "block"
content (Variable v) = ci $ str v
content (NumEntity e) = cn . str $ mathMlOfEntity e
content (Truth t) = cn $ shows t
content (CInteger i) = cn $ shows i
content (CFloat d) = cn $ shows d
content (Meta _ f) = content f
content (Lambda _clauses) = id

content (UnOp op f) =
    apply $ str (stringOfUnOp op)
          . content f

content (BinOp op lst) =
    apply $ str (stringOfBinOp op)
          . concatMapS content lst

content (Product (BinOp OpEq [Variable v, def]) end what) =
    bigOperator "<product/>" v def end what

content (Sum (BinOp OpEq [Variable v, def]) end what) =
    bigOperator "<sum/>" v def end what

content (Matrix _ _ lsts) =
    matrix $ concatS [matrixrow $ concatMapS content row | row <- lsts]

content (Integrate begin end what var) =
    apply $ str "<int/>"
          . bvar (content var)
          . lowlimit (content begin)
          . uplimit (content end)
          . content what

content (Derivate f var) =
    apply $ str "<diff/>"
          . bvar (content var)
          . content f

content (App func args) = 
    apply $ content func
          . concatMapS content args
content _ = id

