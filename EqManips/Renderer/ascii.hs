{-# LANGUAGE ScopedTypeVariables #-}
-- | Module in charge of rendering an equation in ASCII
-- provide sizing information and rendering
module EqManips.Renderer.Ascii( renderFormula
                              , formulaTextTable
                              , formatFormula ) where

import Data.List( foldl' )
import Data.Array.Unboxed
import EqManips.Types
import EqManips.Renderer.Placer
import EqManips.Algorithm.Utils
import EqManips.Propreties

import CharArray
type Pos = (Int, Int)

-- | Here is all the rules for sizing of equation for an ascii
-- rendering. It's a bit harch to look at, but you can look
-- at the test suite to decipher the more complex ones
asciiSizer :: Dimensioner
asciiSizer = Dimensioner
    { unaryDim = \op (base, (w,h)) ->
        let s OpNegate = (base, (w + 1, h))
            s OpFactorial = (base, (w + 1, h))
            s OpAbs = (base, (w + 2, h))
            s OpSqrt = if h == 1
                then (base + 1, (w + 2, h + 1))
                else (base + 1, (w + (h * 3) `div` 2, h + 1))

            s OpExp = (h, (1 + w, 1 + h))
            s OpCeil = (base + 1, (2 + w, 1 + h))
            s OpFloor = (base, (2 + w, 1 + h))
            s OpFrac = (base, (2 + w, h))

            s oper = (h `div` 2, (w + opLength + 2, h))
                where opLength = 
                       case oper `getProp` OperatorText of
                           Just name -> length name
                           Nothing -> error "Unknown operator name"
        in s op

    , varSize = \s -> (0, (length s, 1))
    , intSize = \i -> (0, (length $ show i,1))
    , truthSize = \v -> if v then (0, (length "true", 1))
                             else (0, (length "false", 1))

    , floatSize = \f -> (0, (length $ show f, 1))
    , addParens = \(w, h) -> (w + 2, h)
    , remParens = \(w, h) -> (w - 2, h)
    , divBar = \(_,(w1,h1)) (_,(w2,h2)) ->
                    (h1, (max w1 w2 + 2, h1 + h2 + 1))

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
                    in (base, (w1 + w2 + 2 + oplength, nodeSize))

    , productSize = \(_, (iniw,inih)) (_, (endw,endh)) (_, (whatw,whath)) ->
            let height = inih + endh + max 2 whath
                sumW = maximum [iniw, endw, 3]
                width = sumW + whatw + 1
            in (endh + 1 + whath `div` 2 , (width, height))

    , sumSize = \(_, (iniw,inih)) (_, (endw,endh)) (_, (whatw,whath)) ->
            let height = inih + endh + max 2 whath + 2
                sumW = maximum [iniw, endw, whath, 2]
                width = sumW + whatw + 1
            in (endh + 1 + whath `div` 2 , (width, height))

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
                  (wa + w + 2, max argBase nodeBase, max lower (h-nodeBase))

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

-- | Convert entity to text, not much entity for
-- the moment
textOfEntity :: Entity -> ((Int,(Int,Int)), [String])
textOfEntity Pi = ((0,(2,1)),["pi"])
textOfEntity Infinite = ((0,(length "infinite",1)), ["infinite"])
textOfEntity Nabla = ((1,(2,1)), [" _ ","\\/"])

-- | Little helper for ready to parse string
formatFormula :: Formula -> String
formatFormula = unlines . formulaTextTable

-- | The function to call to render a formula.
-- Return a list of lines containing the formula.
-- You can indent the lines do whatever you want with it.
formulaTextTable :: Formula -> [String]
formulaTextTable = linesOfArray . fst . renderFormula

-------------------------------------------------------------
----                     Rendering                       ----
-------------------------------------------------------------
-- | This function return a char matrix containing the rendered
-- formula. This function might not stay public in the future...
renderFormula :: Formula -- ^ Formula to render
              -> (UArray (Int,Int) Char,SizeTree) -- ^ Rendered formula
renderFormula formula = 
    (accumArray (flip const) ' ' size writeList, sizeTree)
        where sizeTree = sizeTreeOfFormula asciiSizer formula
              size = ((0,0), sizeOfTree sizeTree)
              writeList = renderF formula sizeTree (0,0) []

-- | Same idea as behind ShowS, to avoid heavy concatenation
-- use function composition instead which seem to be cheaper
type PoserS = [(Pos, Char)] -> [(Pos, Char)]

{- else we try to render something like that :
-- @
--     /        \
--     |        |
--     |        |
--     \        /
-- @
-- Kept away from normal haddock comment, because it crash...
-}
-- | One function to render them all! (parenthesis)
-- for one line ( ... )
renderParens :: Pos -> Dimension -> PoserS
renderParens (x,y) (w,1) = ([((x,y), '('), ((x + w - 1, y), ')')] ++)
renderParens (x,y) (w,h) =
    ([((x       , y ), '/' ), ((x       , lastLine), '\\'),
      ((rightCol, y ), '\\'), ((rightCol, lastLine), '/' )] ++)
    . ( concat [ [ ((rightCol, height), '|')
                 , ((x       , height), '|')] | height <- [y+1 .. lastLine - 1] ] ++)
       where rightCol = x + w - 1
             lastLine = y + h - 1

-- | One function to render them all!
-- for one line ( ... )
-- else we try to render something like that :
-- @
-- |¯      ¯|
-- |        |
-- |        |
-- |_      _|
-- @
renderSquareBracket :: Pos -> Dimension -> Bool -> Bool -> PoserS
renderSquareBracket (x,y) (w,1) True True = ([((x,y), '['), ((x + w - 1, y), ']')] ++)
renderSquareBracket (x,y) (w,h) top bottom =
    (upper ++) . (downer ++) . (concat 
           [ [ ((rightCol, height), '|')
             , ((x       , height), '|')] | height <- [y .. lastLine]] ++)
       where rightCol = x + w - 1
             lastLine = y + h - 1
             topSymbols s = [((x + 1   , y ), s), ((rightCol - 1, y ), s)] 
             bottomSymbols s = [((x + 1, lastLine), s), ((rightCol - 1, lastLine ), s)] 
             upper = if top then topSymbols '¯' else []
             downer = if bottom then bottomSymbols '_' else []


{- Just try to get that
-- @
--
--  /
--  |   /   /   {   {
--  |   /   {   {
--  /   \   \
--  \   \
--  |
--  |
--  \
--  @ -}

-- | Hope to render { and } for all sizes
renderBraces :: Pos -> Dimension -> Bool -> Bool -> PoserS
renderBraces (x,y) (w, 1) left right = leftChar . rightChar
    where leftChar = if left then (:) ((x,y), '{') else id
          rightChar = if right then (:) ((x + w - 1, y),'}') else id

renderBraces (x,y) (w, 2) renderLeft renderRight = leftChar . rightChar
    where leftChar = if renderLeft 
                        then (++) [((x,y), '{'), ((x,y+1),'{')] 
                        else id
          right = x + w - 1
          rightChar = if renderRight 
                         then (++) [((right, y),'}'), ((right, y+1), '}')]
                         else id

renderBraces (x,y) (w, 3) renderLeft renderRight = leftChar . rightChar
    where leftChar = if renderLeft 
            then (++) [((x,y), '/'), ((x,y+1),'{'), ((x,y+2),'\\')] 
            else id
          right = x + w - 1
          rightChar = if renderRight
            then (++) [((right, y),'\\'), ((right,y+1), '}'), ((right, y+2),'/')]
            else id

renderBraces (x,y) (w, h) renderLeft renderRight = leftChar . rightChar
    where leftChar = if renderLeft then leftBrace else id
          rightChar = if renderRight then rightBrace else id
          top = (h - 4) `div` 2
          bottomLine = y + h - 1
          right = x + w - 1
          middle = y + top + 1
          leftBrace = (++) [ ((x,y),'/'), ((x, bottomLine),'\\')
                           , ((x, middle), '/'), ((x, middle + 1),'\\')] 
                    . (++) [((x,i), '|')| i <- [y + 1 .. middle - 1]]
                    . (++) [((x,i), '|')| i <- [middle + 2 .. bottomLine - 1]]
          rightBrace = (++) [ ((right,y),'\\'), ((right, bottomLine),'/')
                            , ((right, middle), '\\'), ((right, middle + 1),'/')] 
                     . (++) [((right,i), '|')| i <- [y + 1 .. middle - 1]]
                     . (++) [((right,i), '|')| i <- [middle + 2 .. bottomLine - 1]]

-- | Render a list of arguments, used by lambdas & functions
renderArgs :: Pos -- ^ Where to render the arguments
           -> Int -- ^ The baseline for all the arguments
           -> Int 
           -> [(Formula, SizeTree)] -- ^ Arguments to be rendered
           -> (Int, PoserS) -- ^ Width & charList
renderArgs (x,y) argBase argsMaxHeight mixedList = (xla, params
    . renderParens (x , y) (xla - argBegin, argsMaxHeight))
  where argBegin = x + 1
        (params, (xla,_)) = foldl' write (id, (argBegin,y)) mixedList

        write (acc, (x',y')) (node, size) =
            ( commas . argWrite . acc , (x' + nodeWidth + 2, y') )
              where (nodeWidth, _) = sizeOfTree size
                    commas = (:) ((x' + nodeWidth, y + argBase), ',')
                    nodeBase = baseLineOfTree size
                    baseLine' = y' + (argBase - nodeBase)
                    argWrite = renderF node size (x', baseLine')

-- | The real rendering function, return a list of position and char
-- to be used in accumArray function.
renderF :: Formula -- ^ CurrentNode
        -> SizeTree -- ^ Previously calculated size
        -> Pos -- ^ Where to render
        -> PoserS -- ^ Result to be used in accumArray

-- INVISIBLE META NINJA
renderF (Meta _ f) node pos = renderF f node pos

-- In the following matches, we render parenthesis and
-- then recurse to the normal flow for the regular render.
renderF node (MonoSizeNode True (base, dim) st) (x,y) =
    renderParens (x,y) dim . renderF node neoTree (x+1, y) 
        where subSize = (remParens asciiSizer) dim
              neoTree = MonoSizeNode False (base, subSize) st
-- Parentheses for binop
renderF node (BiSizeNode True (base, dim) st1 st2) (x,y) =
    renderParens (x,y) dim . renderF node neoTree (x+1, y) 
        where subSize = (remParens asciiSizer) dim
              neoTree = BiSizeNode False (base, subSize) st1 st2
-- Parenthesis for something else
renderF node (SizeNodeList True (base, dim) abase stl) (x,y) =
    renderParens (x,y) dim . renderF node neoTree (x+1, y)
        where subSize = (remParens asciiSizer) dim
              neoTree = SizeNodeList False (base, subSize) abase stl

-- Here we make the "simple" rendering, just a conversion.
renderF (Block _ w h) _ (x,y) =
    (++) [ ((xw, yh), '#') | xw <- [x .. x + w - 1], yh <- [y .. y + h - 1]]
renderF (Variable s) _ (x,y) = (++) . map (\(idx,a) -> ((idx,y), a)) $ zip [x..] s
renderF (CInteger i) _ (x,y) = (++) . map (\(idx,a) -> ((idx,y), a)) $ zip [x..] (show i)
renderF (CFloat d)   _ (x,y) = (++) . map (\(idx,a) -> ((idx,y), a)) $ zip [x..] (show d)
renderF (NumEntity e) _ (x,y) = (++) . concat $
    [ [((x + xi,y + yi),c) | (xi, c) <- zip [0..] elines]
        | (yi, elines) <- zip [0..] $ snd $ textOfEntity e]
renderF (Truth True) _ (x,y) = (++) $ map (\(idx, a) -> ((idx,y), a)) $ zip [x..] "true"
renderF (Truth False) _ (x,y) = (++) $ map (\(idx, a) -> ((idx,y), a)) $ zip [x..] "false"
renderF (BinOp _ []) _ _ = error "renderF - rendering BinOp with no operand."
renderF (BinOp _ [_]) _ _ = error "renderF - rendering BinOp with only one operand."

renderF (BinOp OpPow [f1,f2]) (BiSizeNode False _ t1 t2) (x,y) =
    leftRender . rightRender
    where leftRender = renderF f1 t1 (x, y + rh)
          rightRender = renderF f2 t2 (x + lw, y)
          (lw, _) = sizeOfTree t1
          (_, rh) = sizeOfTree t2

-- Division is of another kind :]
renderF (BinOp OpDiv [f1,f2]) (BiSizeNode False (_,(w,_)) t1 t2) (x,y) =
    (++) [ ((xi,y + lh), '-') | xi <- [x .. x + w - 1]] 
    . renderF f1 t1 (leftBegin , y)
    . renderF f2 t2 (rightBegin, y + lh + 1)
        where (lw, lh) = sizeOfTree t1
              (rw, _) = sizeOfTree t2
              leftBegin = x + (w - lw) `div` 2
              rightBegin = x + (w - rw) `div` 2

renderF (BinOp op [f1,f2]) (BiSizeNode False (base,_) t1 t2) (x,y) =
  (++) [ ((i, y + base), c) | (i, c) <- zip [x + lw + 1 ..] opChar]
  . leftRender . rightRender
    where (lw, _) = sizeOfTree t1
          leftBase = baseLineOfTree t1
          rightBase = baseLineOfTree t2
          opChar = binopString op

          (leftTop, rightTop) =
              if leftBase > rightBase
                 then (y, y + leftBase - rightBase)
                 else (y + rightBase - leftBase, y)

          leftRender = renderF f1 t1 (x, leftTop)
          rightRender = renderF f2 t2 (x + lw + 2 + length opChar
                                      , rightTop)

renderF f@(BinOp _ _) node pos = renderF (treeIfyBinOp f) node pos

renderF (UnOp OpSqrt f) (MonoSizeNode _ (_,(w,2)) s) (x,y) =
    (++) [((x, y+1), '\\'), ((x + 1, y + 1), '/')]
    . (++) [ ((i, y), '_') | i <- [x + 2 .. x + w - 1] ]
    . renderF f s (x + 2, y + 1)

renderF (UnOp OpSqrt f) (MonoSizeNode _ (_,(w,h)) s) (x,y) =
    -- The sub formula
    renderF f s (leftBegin, y + 1)
    -- The top line
    . (++) [ ((left,y), '_') | left <- [leftBegin .. x + w - 1] ]
    -- big line from bottom to top
    . (++) [ ((middleMark + i, y + h - i), '/') | i <- [1 .. h - 1] ]
    -- Tiny line from middle to bottom
    . (++) [ ((x + i, halfScreen + i), '\\') | i <- [0 .. midEnd]]
        where (subW,_) = sizeOfTree s
              leftBegin = x + w - subW
              middleMark = leftBegin - h
              halfScreen = y + h `div` 2 + 1
              midEnd = h `div` 2 - 2 + h `mod` 2

renderF (UnOp OpCeil f) (MonoSizeNode _ (_,(w,h)) s) (x,y) =
    renderSquareBracket (x,y) (w,h) True False . renderF f s (x + 1,y + 1)

renderF (UnOp OpFloor f) (MonoSizeNode _ (_,(w,h)) s) (x,y) =
    renderSquareBracket (x,y) (w,h) False True . renderF f s (x + 1,y)

renderF (UnOp OpFrac f) (MonoSizeNode _ (_,(w,h)) s) (x,y) =
    renderBraces (x,y) (w,h) True True . renderF f s (x + 1,y)

renderF (UnOp OpFactorial f) (MonoSizeNode _ (b,(w,_)) s) (x,y) =
    (((x + w - 1, y + b), '!') :) . renderF f s (x,y)

renderF (UnOp OpNegate f) (MonoSizeNode _ (b,_) s) (x,y) =
    (((x,y + b), '-') :) . renderF f s (x + 1,y)

renderF (UnOp OpExp f) (MonoSizeNode _ (_,(_,h)) s) (x,y) =
    (((x, y + h - 1), 'e') :) . renderF f s (x + 1, y)

renderF (UnOp OpAbs f) (MonoSizeNode _ (_,(w,h)) s) (x,y) =
    (++) (concat [  [((x,height), '|'), ((x + w - 1, height), '|')]
                                | height <- [y .. y + h - 1] ])
    . renderF f s (x+1,y)

renderF (UnOp op f) (MonoSizeNode _ nodeSize subSize) (x,y) =
    renderF (App (Variable opName) [f]) 
            (SizeNodeList False nodeSize b 
                    [EndNode(0,(length opName,1)) ,subSize])
            (x,y) 
        where (b,_) = sizeExtract subSize
              opName = op `obtainProp` OperatorText

renderF (App func flist) (SizeNodeList False (base, (_,h)) argBase (s:ts)) 
        (x,y) =
    (snd $ renderArgs (x + fw, y) argBase h mixedList) . renderF func s (x,baseLine) 
        where (fw, _) = sizeOfTree s
              baseLine = y + base
              mixedList = zip flist ts

renderF (Lambda clauses) (SizeNodeClause _ (_,(w,h)) subTrees) (x,y) =
    (fst . foldr renderClause (id, y + 1) . reverse $ zip clauses subTrees)
    . renderBraces (x,y) (w,h) True True
        where renderClause ((args, body), (argBase, trees, _bodyBase, bodyTree))
                           (lst, top) =
                  let (left, rez) = renderArgs (x + 1, top) argBase argsHeight
                                  $ zip args trees
                      bodyText = renderF body bodyTree (left + 3, top)
                      (_, bodyHeight) = sizeOfTree bodyTree
                      argsHeight = maximum [ snd $ sizeOfTree tree | tree <- trees]
                      maxTop = max argsHeight bodyHeight
                      arrow = (++) [ ((left, top + argBase), '-')
                                   , ((left + 1, top + argBase), '>') ]
                  in
                  (arrow . rez . bodyText . lst, maxTop + top + 1)

renderF (Integrate ini end what var)
        (SizeNodeList False
            (_, (w,_h)) _ [iniSize,endSize,whatSize, derVarSize])
        (x,y) =
      renderF end endSize (x + (integWidth - ew) `div` 2, y)
    . renderF ini iniSize (x + (integWidth - iw) `div` 2 - 1, bottom + 1)
    . renderF what whatSize (whatBegin + 1, whatTop)
    . renderF var derVarSize (varBegin + 1, varTop)

    . (++) [ ((integPos, y + eh + 1), '/'), ((integPos + 1, y + eh), '_')
           , ((integPos, bottom),'/'), ((integPos - 1, bottom),'_')
           , ((varBegin, varTop + vh `div` 2), 'd')]

    . (++) [ ((x + 1, i), '|') | i <- [y + eh + 2 .. bottom - 1] ]
        where (ww, wh) = snd $ sizeExtract whatSize
              (ew, eh) = snd $ sizeExtract endSize
              (iw, _) = snd $ sizeExtract iniSize
              (vw, vh) = snd $ sizeExtract derVarSize

              integPos = x + 1 -- (integWidth - 4) `div` 2
              whatTop = y + eh + 1
              varTop = whatTop + (wh - vh) `div` 2

              integWidth = w - 1 - ww - vw
              varBegin = x + w - vw - 1
              whatBegin = varBegin - 2 - ww
              bottom = y + eh + max 2 wh

renderF (Product ini end what)
        (SizeNodeList False
             (_, (w,_h)) _ [iniSize,endSize,whatSize])
        (x,y) =
    renderF end endSize (x + (sumWidth - ew) `div` 2, y)
    . renderF ini iniSize (x + (sumWidth - iw) `div` 2, bottom + 1)
    . renderF what whatSize (whatBegin + 1, y + eh + 1)
    -- Top line
    . (++) [ ((i, y + eh), '_') | i <- [x .. whatBegin - 1]]
    -- Descending line
    . ((++) $ concat [ [((x,i), '|'), ((whatBegin - 1,i), '|')] 
                                   | i <- [ y + eh + 1.. bottom] ])
        where (_, (ww, wh)) = sizeExtract whatSize
              (_, (ew, eh)) = sizeExtract endSize
              (_, (iw, _)) = sizeExtract iniSize
              sumWidth = w - 1 - ww
              whatBegin = x + w - 1 - ww
              bottom = y + eh + max 2 wh
              {-middleStop = wh `div` 2 + if wh `mod` 2 == 0-}
                    {-then -1 else 0-}

renderF (Derivate what var) (BiSizeNode _ (_,(w,_)) whatSize vardSize) (x,y) =
    (++) [((x, y + wh - 1), 'd'), ((x, y + wh + 1), 'd')]
    . (++) [ ((i, y + wh), '-') | i <- [x .. x + w - 1] ]
    . renderF what whatSize (x + 2, y)
    . renderF var vardSize (x + 2, y + wh + 1)
     where (_, (_, wh)) = sizeExtract whatSize

renderF (Sum ini end what)
        (SizeNodeList False
              (_, (w,_h)) _ [iniSize,endSize,whatSize])
        (x,y) =
    renderF end endSize (x + (sumWidth - ew) `div` 2, y)
    . renderF ini iniSize (x + (sumWidth - iw) `div` 2, bottom + 1)
    . renderF what whatSize (whatBegin + 1, y + eh + 1)
    -- Top line
    . (++) [ ((i, y + eh), '_') | i <- [x .. whatBegin - 1]]
    -- Bottom line
    . (++) [ ((i, bottom), '_') | i <- [x .. whatBegin - 1]]
    -- Descending line
    . (++) [ ((x + i, y + eh + 1 + i), '\\') | i <- [0 .. middleStop]]
    -- Ascending line
    . (++) [ ((x + i, bottom - i), '/') | i <- [0 .. middleStop]]
        where (_, (ww, wh)) = sizeExtract whatSize
              (_, (ew, eh)) = sizeExtract endSize
              (_, (iw, _)) = sizeExtract iniSize
              sumWidth = w - 1 - ww
              whatBegin = x + w - 1 - ww
              bottom = y + eh + max 2 wh
              middleStop = wh `div` 2 + if wh `mod` 2 == 0
                    then -1 else 0

renderF (Matrix _n _m subs) (SizeNodeArray _ (_base,(w,h)) lst) (x,y) =
    renderSquareBracket (x,y) (w,h) True True . final
     where renderLine (x', y', acc) (formu, ((base,(w',_)),size)) =
            let (nodeBase, (nodeWidth, _)) = sizeExtract size
                xStart = x' + (w' - nodeWidth) `div` 2
                yStart = y' + (base - nodeBase)
            in
            (x' + w' + 1, y', renderF formu size (xStart, yStart) . acc)
           
           renderMatrix (x', y', acc) (formulas, sizes) = 
               let ((_,(_,height)),_) = head sizes
                   (_,_, acc') = foldl' renderLine (x', y', acc) $ zip formulas sizes
               in
               (x', y' + height + 1, acc')

           (_,_, final) = foldl' renderMatrix (x + 2, y + 1, id) $ zip subs lst

renderF _ _ _ = error "renderF - unmatched case"

