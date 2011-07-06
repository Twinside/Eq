module Language.Eq.Renderer.Mathml( mathmlRender ) where

import Language.Eq.Types hiding ( matrix )
import Language.Eq.Algorithm.Utils
import Language.Eq.Propreties

import Language.Eq.Renderer.Latex
import Language.Eq.Renderer.EqCode
import Language.Eq.Renderer.RenderConf

mathmlRender :: Conf -> Formula TreeForm -> String
mathmlRender conf (Formula f) =
    str "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    . semantics ( presMarkup 
                . annotation "MathML-Content" contentMarkup
                . annotation "Eq-language" (str . cleanify $ unparse f)
                . annotation "LaTeX" (str . cleanify . latexRender conf $ Formula f))
    . str "</math>\n" $ ""
        where contentMarkup = content f
              presMarkup = mrow $ prez conf f
              semantics = tagger "semantics"
              annotation kind c =
                  str ("<annotation-xml encoding=\"" ++ kind ++ "\">\n")
                           . c . str "\n</annotation-xml>\n"

str :: String -> ShowS
str = (++)

char :: Char -> ShowS
char = (:)

mathMlOfEntity :: Entity -> String
mathMlOfEntity Pi = "<pi/>"
mathMlOfEntity Nabla = "<grad/>"
mathMlOfEntity Infinite = "<infinity/>"
mathMlOfEntity Ellipsis = "&ctdot;"

tagger :: String -> ShowS -> ShowS
tagger tag f = str ('<': tag ++ ">") . f . str ("</" ++ tag ++ ">")

cleanify :: String -> String
cleanify = concatMap deAnchor
    where deAnchor '<' = "&lt;"
          deAnchor '>' = "&gt;"
          deAnchor '&' = "&amp;"
          deAnchor a = [a]

mo, msup, mi, mn, mfrac, mrow, parens,
    msubsup, msqrt, mfenced, mtable,
    mtd, mtr, msub :: ShowS -> ShowS
mo = tagger "mo"
mi = tagger "mi"
mn = tagger "mn"
mfrac = tagger "mfrac"
mrow = tagger "mrow"
parens f = str "<mo>(</mo>" . f . str "<mo>)</mo>"
msubsup = tagger "msubsup"
msup = tagger "msup"
msub = tagger "msub"
msqrt = tagger "msqrt"

mfenced f = str "<mfenced open=\"[\" close=\"]\">\n" . f . str "</mfenced>\n"
mtable = tagger "mtable"
mtd = tagger "mtd"
mtr = tagger "mtr"

enclose :: Char -> Char -> ShowS -> ShowS
enclose beg end f = str ("<mo>" ++ (beg : "</mo>")) . f . str ("<mo>" ++ (end : "</mo>"))

prez :: Conf -> FormulaPrim -> ShowS
prez conf = presentation conf Nothing

--centerdot
--
presentation :: Conf -> Maybe (BinOperator, Bool) -> FormulaPrim -> ShowS
presentation _ _ (Block _ _ _) = mi $ str "block"
presentation _ _ (Variable v) = mi $ str v
presentation _ _ (NumEntity e) = mn $ str $ mathMlOfEntity e
presentation _ _ (Truth t) = mn $ shows t
presentation _ _ (CInteger i) = mn $ shows i
presentation _ _ (CFloat d) = mn $ shows d
presentation conf inf (Meta _ _ f) = presentation conf inf f
presentation _ _ (Lambda _ _clauses) = id

presentation conf _ (BinOp _ OpPow [a,b]) =
    msup $ mrow (presentation conf (Just (OpPow, False)) a)
         . mrow (presentation conf (Just (OpPow, True)) b)

presentation conf _ (BinOp _ OpDiv [a,b]) =
    mfrac $ mrow (prez conf a)
          . mrow (prez conf b)

presentation conf (Just (pop,isRight)) f@(BinOp _ op _)
    | needParenthesis isRight pop op = parens $ prez conf f
    | otherwise = prez conf f

presentation conf Nothing (BinOp _ OpMul [a,b])
    | mulAsDot conf = presentation conf (Just (OpMul, False)) a
                    . mo (str "&centerdot;")
                    . presentation conf (Just (OpMul, True)) b

    | otherwise = presentation conf (Just (OpMul, False)) a
                . mo (str "&times;")
                . presentation conf (Just (OpMul, True)) b

presentation conf Nothing (BinOp _ op [a,b]) =
    presentation conf (Just (op, False)) a
    . mo (str . cleanify $ binopString op)
    . presentation conf (Just (op, True)) b

-- Unary operators
presentation conf _ (UnOp _ OpCeil f) = str "<mo>&lceil;</mo>"
                                      . prez conf f 
                                      . str "<mo>&rceil;</mo>"
presentation conf _ (UnOp _ OpFloor f) = str "<mo>&lfloor;</mo>"
                                       . prez conf f 
                                       . str "<mo>&rfloor;</mo>"
presentation conf _ (UnOp _ OpFrac f) = enclose '{' '}' $ prez conf f
presentation conf _ (UnOp _ OpAbs f) = enclose '|' '|' $ prez conf f
presentation conf _ (UnOp _ OpSqrt f) = msqrt $ prez conf f
presentation conf _ (UnOp _ OpFactorial f)
  | f `hasProp` LeafNode = prez conf f . mo (char '!')
  | otherwise = parens (prez conf f) . mo (char '!')
presentation conf _ (UnOp _ OpNegate f)
  | f `hasProp` LeafNode = mo (char '-') . prez conf f
  | otherwise = mo (char '-') . parens (prez conf f)
presentation conf _ (UnOp _ op f)
  | f `hasProp` LeafNode = mo (str $ unopString op) . prez conf f
  | otherwise = mo (str $ unopString op) . parens (prez conf f)

presentation conf _ (Sum _ begin end what) =
     msubsup ( mo (str "&sum;")
             . mrow (prez conf begin)
             . mrow (prez conf end)) . mrow (prez conf what)

presentation conf _ (Product _ begin end what) =
     msubsup ( mo (str "&prod;")
             . mrow (prez conf begin)
             . mrow (prez conf end)) . mrow (prez conf what)

presentation conf _ (Integrate _ begin end what var) =
     msubsup ( mo (str "&int;")
             . mrow (prez conf begin)
             . mrow (prez conf end))
             . mrow (prez conf what . mi (str "d") . prez conf var)

presentation conf _ (Derivate _ f var) =
     mfrac ( mi (char 'd')
           . mrow (mi (char 'd') . prez conf var)) . prez conf f

presentation conf _ (App _ func args) =
    prez conf func . parens (interspereseS (mo $ char ',') $ map (prez conf) args)

presentation conf _ (Matrix _ _ _ lsts) =
    mfenced $ mtable $ concatS [mtr $ concatS [ mtd $ prez conf cell | cell <- row] | row <- lsts ]

presentation conf _ (Indexes _ src im) =
    msub ( prez conf src
         . (interspereseS (mo $ char ',') $ map (prez conf) im)
         )

presentation _conf _ (Fraction _f) = id
presentation _conf _ (List _ _im) = id
presentation _conf _ (Complex _ _im) = id

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
stringOfUnOp OpNegate = "<minus/>"
stringOfUnOp OpFrac = "<ci>frac</ci>"
stringOfUnOp OpMatrixWidth = "matrixWidth"
stringOfUnOp OpMatrixHeight = "matrixHeight"

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
stringOfBinOp OpAttrib = "<!-- Attrib -->"
stringOfBinOp OpLazyAttrib = "<!-- LazyAttrib -->"
stringOfBinOp OpCons = "<!-- Cons -->"

bigOperator :: String -> String -> FormulaPrim -> FormulaPrim -> FormulaPrim
            -> ShowS
bigOperator operator var def end what = 
    apply $ str operator
          . bvar (str var)
          . lowlimit (content def)
          . uplimit (content end)
          . content what

-- | Give 2 xml trees, one for presentation and one
-- for content. Shitty MathML.
content :: FormulaPrim -> ShowS
content (Block _ _ _) = ci $ str "block"
content (Variable v) = ci $ str v
content (NumEntity e) = cn . str $ mathMlOfEntity e
content (Truth True) = str "<true/>"
content (Truth False) = str "<false/>"
content (CInteger i) = cn $ shows i
content (CFloat d) = cn $ shows d
content (Meta _ _ f) = content f
content (Lambda _ _clauses) = id

content (UnOp _ op f) =
    apply $ str (stringOfUnOp op)
          . content f

content (BinOp _ op lst) =
    apply $ str (stringOfBinOp op)
          . concatMapS content lst

content (Product _ (BinOp _ OpEq [Variable v, def]) end what) =
    bigOperator "<product/>" v def end what

content (Sum _ (BinOp _ OpEq [Variable v, def]) end what) =
    bigOperator "<sum/>" v def end what

content (Matrix _ _ _ lsts) =
    matrix $ concatS [matrixrow $ concatMapS content row | row <- lsts]

content (Integrate _ begin end what var) =
    apply $ str "<int/>"
          . bvar (content var)
          . lowlimit (content begin)
          . uplimit (content end)
          . content what

content (Derivate _ f var) =
    apply $ str "<diff/>"
          . bvar (content var)
          . content f

content (App _ func args) = 
    apply $ content func
          . concatMapS content args
content _ = id

