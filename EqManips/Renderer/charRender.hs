module EqManips.Renderer.CharRender( CharacterSoup, CharacterSoupS
								   , renderFormula, renderFormulaS
								   ) where

{-import Data.List( foldl' )-}
import EqManips.Types
import EqManips.Renderer.Placer
{-import EqManips.Algorithm.Utils-}
import EqManips.Propreties

type PosX = Int
type PosY = Int
type Width = Int
type Height = Int
type CharacterSoup = [(PosX, PosY, Width, Height, Char)]
type CharacterSoupS = CharacterSoup -> CharacterSoup 

type Pos = (PosX, PosY)

textOfEntity :: Entity -> ((Int,(Int,Int)), [String])
textOfEntity Pi = ((0,(2,1)),["pi"])
textOfEntity Infinite = ((0,(length "infinite",1)), ["infinite"])
textOfEntity Nabla = ((1,(2,1)), [" _ ","\\/"])

--------------------------------------------------
----            API
--------------------------------------------------
renderFormula :: Formula -> CharacterSoup
renderFormula f = renderFormulaS f []

renderFormulaS :: Formula -> CharacterSoupS
renderFormulaS f = render f formulaSize (0,0)
	where formulaSize = sizeTreeOfFormula charSizer f

--------------------------------------------------
----            Constants
--------------------------------------------------
baseCell :: Int
baseCell = 65536

parensWidth :: Int
parensWidth = baseCell `div` 4

opSpace :: Int
opSpace = baseCell `div` 6 

divbarWidthAdd :: Int
divbarWidthAdd = baseCell `div` 10

commaSize :: Int
commaSize = baseCell

--------------------------------------------------
----            Implementation
--------------------------------------------------
-- | Sizer for the real equation formatting.
-- Hardly readable, but get job done.
charSizer :: Dimensioner
charSizer = Dimensioner
    { unaryDim = \op (base, (w,h)) ->
        let s OpNegate = (base, (w + baseCell, h))
            s OpFactorial = (base, (w + baseCell, h))
            s OpAbs = (base, (w + 2 * baseCell, h))
            s OpSqrt = (base + 1, (w + (h * 3) `div` 2, h + 1)) 
            s OpExp = (h, (baseCell + w, baseCell + h))
            s OpCeil = (base + baseCell, (2 * baseCell+ w, baseCell + h))
            s OpFloor = (base, (2 * baseCell + w, baseCell + h))
            s OpFrac = (base, (2 * baseCell + w, h))

            s oper = (h `div` 2, (w + opLength + 2 * baseCell, h))
                where opLength = 
                       case oper `getProp` OperatorText of
                           Just name -> length name * baseCell
                           Nothing -> error "Unknown operator name"
        in s op

    , varSize = \s -> (baseCell, (length s * baseCell, baseCell))
    , intSize = \i -> (baseCell, (length (show i) * baseCell, baseCell))
    , truthSize = \v -> if v then (baseCell, (baseCell * length "true", baseCell))
                             else (baseCell, (baseCell * length "false", baseCell))

    , floatSize = \f -> (baseCell, (length (show f) * baseCell, baseCell))

	--------------------------------------------------
    ----            Parenthesis
    --------------------------------------------------
    , addParens = \(w, h) -> (w + parensWidth * 2, h)
    , remParens = \(w, h) -> (w - parensWidth * 2, h)

    , divBar = \(_,(w1,h1)) (_,(w2,h2)) ->
                    (h1, (max w1 w2 + 2 * divbarWidthAdd, h1 + h2 + 1))

    , powSize = \(b,(w1,h1)) (_,(w2,h2)) ->
                    (b + h2, (w1 + w2, h1 + h2))

      -- We must handle case like this :
      --  +-------+
      --  |       |+-------+
      --  +-------|+-------+
      --  |       ||       |
      --  +-------+|       |
      --           +-------+
    , binop = \op (bl,(w1,h1)) (br,(w2,h2)) ->
                    let base = max bl br
                        oplength = length $ binopString op
                        nodeSize = base + max (h1 - bl) (h2 - br)
                    in (base, (w1 + w2 + 2 * opSpace + oplength, nodeSize))

    , productSize = \(_, (iniw,inih)) (_, (endw,endh)) (_, (whatw,whath)) ->
            let height = inih + endh + max 2 whath
                sumW = maximum [iniw, endw, 3]
                width = sumW + whatw + 1
            in (endh + 1 + whath `div` 2 , (width, height))

    , sumSize = \(_, (iniw,inih)) (_, (endw,endh)) (_, (whatw,whath)) ->
            let height = inih + endh + max (2 * baseCell) whath + (2 * baseCell)
                sumW = maximum [iniw, endw, whath, (2 * baseCell)]
                width = sumW + whatw + baseCell
            in (endh + baseCell + whath `div` (2 * baseCell), (width, height))

    , integralSize = \(_, (iniw,inih)) (_, (endw,endh)) (_, (whatw,whath)) 
                      (_, (dvarw, dvarh))->
            let height = inih + endh + maximum [2, dvarh, whath] + 2
                sumW = maximum [iniw, endw, whath, 4]
                width = sumW + whatw + 2 + dvarw
            in (endh + 1 + whath `div` 2 , (width, height))

    , matrixSize = \lst ->
        let mHeight = sum [ h | (_,(_,h)) <- map head lst ]
                      + length lst
                      + 1
            firstLine = head lst
            mWidth = length firstLine + sum [ w | (_,(w,_)) <- firstLine ]
        in
        (mHeight `div` 2, (mWidth + 3, mHeight))

    , derivateSize = \(_,(we,he)) (_,(wv, hv)) ->
        (he, (max we wv + 3, he + hv + 1))

    , blockSize = \(i1,i2,i3) -> (i1, (i2,i3))
    , entitySize = fst . textOfEntity

    , argSize = \(wa, argBase, lower) (nodeBase, (w,h)) ->
                  (wa + w + commaSize, max argBase nodeBase, max lower (h-nodeBase))

    , appSize = \(pw, argsBase, argsLeft) (_, (wf, hf)) ->
            let finalY = max hf (argsBase + argsLeft)
            in ((finalY - hf) `div` 2, (wf + pw, finalY))

    -- lambdaSize :: [((Int,Int,Int), RelativePlacement)] -> RelativePlacement
    , lambdaSize = \poses -> 
        let clauseCount = length poses
            mHeight = 2 + clauseCount + sum
                [ max bodyH $ top + bottom | ((_, top, bottom), (_,(_,bodyH))) <- poses ]
            mWidth = maximum
                [ w + 4 {- " -> " -} + bodyW 
                    | ((w, _, _), (_,(bodyW,_))) <- poses]
        in
        (mHeight `div` 2, (2 + mWidth, mHeight))
    }

render :: Formula -> SizeTree -> Pos -> CharacterSoupS
render (Meta _ f) node pos = render f node pos

-- In the following matches, we render parenthesis and
-- then recurse to the normal flow for the regular render.
{-render node (MonoSizeNode True (base, dim) st) (x,y) =-}
{--- Parentheses for binop-}
{-render node (BiSizeNode True (base, dim) st1 st2) (x,y) =-}
{--- Parenthesis for something else-}
{-render node (SizeNodeList True (base, dim) abase stl) (x,y) =-}

{--- Here we make the "simple" rendering, just a conversion.-}
{-render (Block _ w h) _ (x,y) =-}
{-render (Variable s) _ (x,y) =-}
{-render (CInteger i) _ (x,y) =-}
{-render (CFloat d)   _ (x,y) =-}
{-render (NumEntity e) _ (x,y) =-}
    {-[ [((x + xi,y + yi),c) | (xi, c) <- zip [0..] elines]-}
        {-| (yi, elines) <- zip [0..] $ snd $ textOfEntity e]-}
{-render (Truth True) _ (x,y) =-}
{-render (Truth False) _ (x,y) =-}
{-render (BinOp _ []) _ _ = error "render - rendering BinOp with no operand."-}
{-render (BinOp _ [_]) _ _ = error "render - rendering BinOp with only one operand."-}

{-render (BinOp OpPow [f1,f2]) (BiSizeNode False _ t1 t2) (x,y) =-}
{--- Division is of another kind :]-}
{-render (BinOp OpDiv [f1,f2]) (BiSizeNode False (_,(w,_)) t1 t2) (x,y) =-}
{-render (BinOp op [f1,f2]) (BiSizeNode False (base,_) t1 t2) (x,y) =-}
{-render f@(BinOp _ _) node pos = render (treeIfyBinOp f) node pos-}
{-render (UnOp OpSqrt f) (MonoSizeNode _ (_,(w,2)) s) (x,y) =-}
{-render (UnOp OpSqrt f) (MonoSizeNode _ (_,(w,h)) s) (x,y) =-}
{-render (UnOp OpCeil f) (MonoSizeNode _ (_,(w,h)) s) (x,y) =-}
{-render (UnOp OpFloor f) (MonoSizeNode _ (_,(w,h)) s) (x,y) =-}
{-render (UnOp OpFrac f) (MonoSizeNode _ (_,(w,h)) s) (x,y) =-}
{-render (UnOp OpFactorial f) (MonoSizeNode _ (b,(w,_)) s) (x,y) =-}
{-render (UnOp OpNegate f) (MonoSizeNode _ (b,_) s) (x,y) =-}
{-render (UnOp OpExp f) (MonoSizeNode _ (_,(_,h)) s) (x,y) =-}
{-render (UnOp OpAbs f) (MonoSizeNode _ (_,(w,h)) s) (x,y) =-}
{-render (UnOp op f) (MonoSizeNode _ nodeSize subSize) (x,y) =-}
{-render (App func flist) (SizeNodeList False (base, (_,h)) argBase (s:ts)) -}
        {-(x,y) =-}
{-render (Lambda clauses) (SizeNodeClause _ (_,(w,h)) subTrees) (x,y) =-}
{-render (Integrate ini end what var)-}
        {-(SizeNodeList False-}
            {-(_, (w,_h)) _ [iniSize,endSize,whatSize, derVarSize])-}
        {-(x,y) =-}
{-render (Product ini end what)-}
        {-(SizeNodeList False-}
             {-(_, (w,_h)) _ [iniSize,endSize,whatSize])-}
        {-(x,y) =-}
{-render (Derivate what var) (BiSizeNode _ (_,(w,_)) whatSize vardSize) (x,y) =-}
{-render (Sum ini end what)-}
        {-(SizeNodeList False-}
              {-(_, (w,_h)) _ [iniSize,endSize,whatSize])-}
        {-(x,y) =-}
{-render (Matrix _n _m subs) (SizeNodeArray _ (_base,(w,h)) lst) (x,y) =-}
render _ _ _ = error "render - unmatched case"

