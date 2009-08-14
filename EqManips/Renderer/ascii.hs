{-# LANGUAGE ScopedTypeVariables #-}
-- | Module in charge of rendering an equation in ASCII
-- provide sizing information and rendering
module EqManips.Renderer.Ascii( renderFormula
                              , formulaTextTable
                              , formatFormula ) where

import Control.Monad( foldM )
import Data.List( foldl' )
import Data.Array.Unboxed
import EqManips.Types
import EqManips.Renderer.Placer
import EqManips.Algorithm.Utils
import EqManips.Propreties
import Monad.ListProducer

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
    , binop = \_ (bl,(w1,h1)) (br,(w2,h2)) ->
                    let base = max bl br
                        nodeSize = base + max (h1 - bl) (h2 - br)
                    in (base, (w1 + w2 + 3, nodeSize))

    , argSize = \(wa, argBase, lower) (nodeBase, (w,h)) ->
                  (wa + w + 2, max argBase nodeBase, max lower (h-nodeBase))

    , appSize = \(pw, argsBase, argsLeft) (_, (wf, hf)) ->
            let finalY = max hf (argsBase + argsLeft)
            in ((finalY - hf) `div` 2, (wf + pw, finalY))

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
        where sizeTree = sizeOfFormula asciiSizer False maxPrio formula
              size = ((0,0), sizeOfTree sizeTree)
              writeList = renderF formula sizeTree (0,0) 
           
-- | One function to render them all! (parenthesis)
-- for one line ( ... )
-- else we try to render something like that :
-- @
-- /        \
-- |        |
-- |        |
-- \        /
-- @
renderParens :: Pos -> Dimension -> [(Pos, Char)]
renderParens (x,y) (w,1) = [((x,y), '('), ((x + w - 1, y), ')')]
renderParens (x,y) (w,h) =
    ((x       , y ), '/' ) : ((x       , lastLine), '\\') :
    ((rightCol, y ), '\\') : ((rightCol, lastLine), '/' ) :
    concat [ [ ((rightCol, height), '|')
             , ((x       , height), '|')] | height <- [y+1 .. lastLine - 1] ]
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
renderSquareBracket :: Pos -> Dimension -> Bool -> Bool -> [(Pos,Char)]
renderSquareBracket (x,y) (w,1) True True = [((x,y), '['), ((x + w - 1, y), ']')]
renderSquareBracket (x,y) (w,h) top bottom =
    upper ++ downer ++
    concat [ [ ((rightCol, height), '|')
             , ((x       , height), '|')] | height <- [y .. lastLine]]
       where rightCol = x + w - 1
             lastLine = y + h - 1
             topSymbols s = [((x + 1   , y ), s), ((rightCol - 1, y ), s)] 
             bottomSymbols s = [((x + 1, lastLine), s), ((rightCol - 1, lastLine ), s)] 
             upper = if top then topSymbols '¯' else []
             downer = if bottom then bottomSymbols '_' else []


-- | Hope to render this :
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
--  @
renderBraces :: Pos -> Dimension -> Bool -> Bool -> [(Pos, Char)]
renderBraces (x,y) (w, 1) left right =leftChar ++ rightChar
    where leftChar = if left then [((x,y), '{')] else []
          rightChar = if right then [((x + w - 1, y),'}')] else []

renderBraces (x,y) (w, 2) renderLeft renderRight = leftChar ++ rightChar
    where leftChar = if renderLeft 
                        then [((x,y), '{'), ((x,y+1),'{')] 
                        else []
          right = x + w - 1
          rightChar = if renderRight 
                         then [((right, y),'}'), ((right, y+1), '}')]
                         else []

renderBraces (x,y) (w, 3) renderLeft renderRight = leftChar ++ rightChar
    where leftChar = if renderLeft 
            then [((x,y), '/'), ((x,y+1),'{'), ((x,y+2),'\\')] 
            else []
          right = x + w - 1
          rightChar = if renderRight
            then [((right, y),'\\'), ((right,y+1), '}'), ((right, y+2),'/')]
            else []

renderBraces (x,y) (w, h) renderLeft renderRight = leftChar ++ rightChar
    where leftChar = if renderLeft then leftBrace else []
          rightChar = if renderRight then rightBrace else []
          top = (h - 4) `div` 2
          bottomLine = y + h - 1
          right = x + w - 1
          middle = y + top + 1
          leftBrace = [ ((x,y),'/'), ((x, bottomLine),'\\')
                      , ((x, middle), '/'), ((x, middle + 1),'\\')] 
                    ++ [((x,i), '|')| i <- [y + 1 .. middle - 1]]
                    ++ [((x,i), '|')| i <- [middle + 2 .. bottomLine - 1]]
          rightBrace = [ ((right,y),'\\'), ((right, bottomLine),'/')
                      , ((right, middle), '\\'), ((right, middle + 1),'/')] 
                    ++ [((right,i), '|')| i <- [y + 1 .. middle - 1]]
                    ++ [((right,i), '|')| i <- [middle + 2 .. bottomLine - 1]]

-- | Little association...
charOfOp :: BinOperator -> Char
charOfOp OpEq = '='
charOfOp OpAdd = '+'
charOfOp OpSub = '-'
charOfOp OpMul = '*'
charOfOp OpDiv = '/'
charOfOp OpPow = '^'

-- | The real rendering function, return a list of position and char
-- to be used in accumArray function.
renderF :: Formula -- ^ CurrentNode
        -> SizeTree -- ^ Previously calculated size
        -> Pos -- ^ Where to render
        -> [(Pos,Char)] -- ^ Result to be used in accumArray

-- INVISIBLE META NINJA
renderF (Meta _ f) node pos = renderF f node pos

-- In the following matches, we render parenthesis and
-- then recurse to the normal flow for the regular render.
renderF node (MonoSizeNode True (base, dim) st) (x,y) =
    renderParens (x,y) dim ++ renderF node neoTree (x+1, y) 
        where subSize = (remParens asciiSizer) dim
              neoTree = MonoSizeNode False (base, subSize) st
-- Parentheses for binop
renderF node (BiSizeNode True (base, dim) st1 st2) (x,y) =
    renderParens (x,y) dim ++ renderF node neoTree (x+1, y) 
        where subSize = (remParens asciiSizer) dim
              neoTree = BiSizeNode False (base, subSize) st1 st2
-- Parenthesis for something else
renderF node (SizeNodeList True (base, dim) abase stl) (x,y) =
    renderParens (x,y) dim ++ renderF node neoTree (x+1, y)
        where subSize = (remParens asciiSizer) dim
              neoTree = SizeNodeList False (base, subSize) abase stl

renderF (Lambda _) _ pos = [(pos, '\\')]

-- Here we make the "simple" rendering, just a conversion.
renderF (Block _ w h) _ (x,y) =
    [ ((xw, yh), '#') | xw <- [x .. x + w - 1], yh <- [y .. y + h - 1]]
renderF (Variable s) _ (x,y) = map (\(idx,a) -> ((idx,y), a)) $ zip [x..] s
renderF (CInteger i) _ (x,y) = map (\(idx,a) -> ((idx,y), a)) $ zip [x..] (show i)
renderF (CFloat d)   _ (x,y) = map (\(idx,a) -> ((idx,y), a)) $ zip [x..] (show d)
renderF (NumEntity e) _ (x,y) = concat
    [ [((x + xi,y + yi),c) | (xi, c) <- zip [0..] elines]
        | (yi, elines) <- zip [0..] $ snd $ textOfEntity e]

renderF (BinOp _ []) _ _ = error "renderF - rendering BinOp with no operand."
renderF (BinOp _ [_]) _ _ = error "renderF - rendering BinOp with only one operand."

renderF (BinOp OpPow [f1,f2]) (BiSizeNode False _ t1 t2) (x,y) =
    leftRender ++ rightRender
    where leftRender = renderF f1 t1 (x, y + rh)
          rightRender = renderF f2 t2 (x + lw, y)
          (lw, _) = sizeOfTree t1
          (_, rh) = sizeOfTree t2

-- Division is of another kind :]
renderF (BinOp OpDiv [f1,f2]) (BiSizeNode False (_,(w,_)) t1 t2) (x,y) =
    [ ((xi,y + lh), '-') | xi <- [x .. x + w - 1]] 
    ++ renderF f1 t1 (leftBegin , y)
    ++ renderF f2 t2 (rightBegin, y + lh + 1)
        where (lw, lh) = sizeOfTree t1
              (rw, _) = sizeOfTree t2
              leftBegin = x + (w - lw) `div` 2
              rightBegin = x + (w - rw) `div` 2

renderF (BinOp op [f1,f2]) (BiSizeNode False (base,_) t1 t2) (x,y) =
  ((x + lw + 1, y + base),opChar) : (leftRender ++ rightRender)
    where (lw, _) = sizeOfTree t1
          leftBase = baseLineOfTree t1
          rightBase = baseLineOfTree t2
          opChar = charOfOp op

          (leftTop, rightTop) =
              if leftBase > rightBase
                 then (y, y + leftBase - rightBase)
                 else (y + rightBase - leftBase, y)

          leftRender = renderF f1 t1 (x, leftTop)
          rightRender = renderF f2 t2 (x + lw + 3, rightTop)

renderF f@(BinOp _ _) node pos = renderF (treeIfyBinOp f) node pos

renderF (UnOp OpSqrt f) (MonoSizeNode _ (_,(w,2)) s) (x,y) =
    ((x, y+1), '\\') : ((x + 1, y + 1), '/')
    : [ ((i, y), '_') | i <- [x + 2 .. x + w - 1] ]
    ++ renderF f s (x + 2, y + 1)

renderF (UnOp OpSqrt f) (MonoSizeNode _ (_,(w,h)) s) (x,y) =
    -- The sub formula
    renderF f s (leftBegin, y + 1)
    -- The top line
    ++ [ ((left,y), '_') | left <- [leftBegin .. x + w - 1] ]
    -- big line from bottom to top
    ++ [ ((middleMark + i, y + h - i), '/') | i <- [1 .. h - 1] ]
    -- Tiny line from middle to bottom
    ++ [ ((x + i, halfScreen + i), '\\') | i <- [0 .. midEnd]]
        where (subW,_) = sizeOfTree s
              leftBegin = x + w - subW
              middleMark = leftBegin - h
              halfScreen = y + h `div` 2 + 1
              midEnd = h `div` 2 - 2 + h `mod` 2

renderF (UnOp OpCeil f) (MonoSizeNode _ (_,(w,h)) s) (x,y) =
    renderSquareBracket (x,y) (w,h) True False ++ renderF f s (x + 1,y + 1)

renderF (UnOp OpFloor f) (MonoSizeNode _ (_,(w,h)) s) (x,y) =
    renderSquareBracket (x,y) (w,h) False True ++ renderF f s (x + 1,y)

renderF (UnOp OpFrac f) (MonoSizeNode _ (_,(w,h)) s) (x,y) =
    renderBraces (x,y) (w,h) True True ++ renderF f s (x + 1,y)

renderF (UnOp OpFactorial f) (MonoSizeNode _ (b,(w,_)) s) (x,y) =
    ((x + w - 1, y + b), '!') : renderF f s (x,y)

renderF (UnOp OpNegate f) (MonoSizeNode _ (b,_) s) (x,y) =
    ((x,y + b), '-') : renderF f s (x,y)

renderF (UnOp OpExp f) (MonoSizeNode _ (_,(_,h)) s) (x,y) =
    ((x, y + h - 1), 'e') : renderF f s (x + 1, y)

renderF (UnOp OpAbs f) (MonoSizeNode _ (_,(w,h)) s) (x,y) =
    concat [  [((x,height), '|'), ((x + w - 1, height), '|')]
             | height <- [y .. y + h - 1] ]
    ++ renderF f s (x+1,y)

renderF (UnOp op f) (MonoSizeNode _ nodeSize subSize) (x,y) =
    renderF (App (Variable opName) [f]) 
            (SizeNodeList False nodeSize b 
                    [EndNode(0,(length opName,1)) ,subSize])
            (x,y) 
        where (b,_) = sizeExtract subSize
              opName = op `obtainProp` OperatorText

renderF (App func flist) (SizeNodeList False (base, (_,h)) argBase (s:ts)) 
        (x,y) =
    concat params
    ++ renderF func s (x,baseLine) 
    ++ renderParens (x + fw, y) (xla - argBegin, h)
        where (fw, _) = sizeOfTree s

              baseLine = y + base

              mixedList = zip flist ts
              argBegin = x + fw + 1
              (params, (xla,_)) = foldl' write ([], (argBegin,y)) mixedList

              write (acc, (x',y')) (node, size) =
                  ( commas : argWrite : acc , (x' + nodeWidth + 2, y') )
                    where (nodeWidth, _) = sizeOfTree size
                          commas = [((x' + nodeWidth, y + argBase), ',')] 
                          nodeBase = baseLineOfTree size
                          baseLine' = y' + (argBase - nodeBase)
                          argWrite = renderF node size (x', baseLine')

renderF (Integrate ini end what var)
        (SizeNodeList False
            (_, (w,_h)) _ [iniSize,endSize,whatSize, derVarSize])
        (x,y) =
       renderF end endSize (x + (integWidth - ew) `div` 2, y)
    ++ renderF ini iniSize (x + (integWidth - iw) `div` 2 - 1, bottom + 1)
    ++ renderF what whatSize (whatBegin + 1, whatTop)
    ++ renderF var derVarSize (varBegin + 1, varTop)

    ++ [ ((integPos, y + eh + 1), '/'), ((integPos + 1, y + eh), '_')
       , ((integPos, bottom),'/'), ((integPos - 1, bottom),'_')
       , ((varBegin, varTop + vh `div` 2), 'd')]

    ++ [ ((x + 1, i), '|') | i <- [y + eh + 2 .. bottom - 1] ]
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
    ++ renderF ini iniSize (x + (sumWidth - iw) `div` 2, bottom + 1)
    ++ renderF what whatSize (whatBegin + 1, y + eh + 1)
    -- Top line
    ++ [ ((i, y + eh), '_') | i <- [x .. whatBegin - 1]]
    -- Descending line
    ++ concat [ [((x,i), '|'), ((whatBegin - 1,i), '|')] 
                    | i <- [ y + eh + 1.. bottom] ]
        where (_, (ww, wh)) = sizeExtract whatSize
              (_, (ew, eh)) = sizeExtract endSize
              (_, (iw, _)) = sizeExtract iniSize
              sumWidth = w - 1 - ww
              whatBegin = x + w - 1 - ww
              bottom = y + eh + max 2 wh
              {-middleStop = wh `div` 2 + if wh `mod` 2 == 0-}
                    {-then -1 else 0-}

renderF (Derivate what var) (BiSizeNode _ (_,(w,_)) whatSize vardSize) (x,y) =
    ((x, y + wh - 1), 'd') : ((x, y + wh + 1), 'd')
    : [ ((i, y + wh), '-') | i <- [x .. x + w - 1] ]
    ++ renderF what whatSize (x + 2, y)
    ++ renderF var vardSize (x + 2, y + wh + 1)
     where (_, (_, wh)) = sizeExtract whatSize

renderF (Sum ini end what)
        (SizeNodeList False
              (_, (w,_h)) _ [iniSize,endSize,whatSize])
        (x,y) =
    renderF end endSize (x + (sumWidth - ew) `div` 2, y)
    ++ renderF ini iniSize (x + (sumWidth - iw) `div` 2, bottom + 1)
    ++ renderF what whatSize (whatBegin + 1, y + eh + 1)
    -- Top line
    ++ [ ((i, y + eh), '_') | i <- [x .. whatBegin - 1]]
    -- Bottom line
    ++ [ ((i, bottom), '_') | i <- [x .. whatBegin - 1]]
    -- Descending line
    ++ [ ((x + i, y + eh + 1 + i), '\\') | i <- [0 .. middleStop]]
    -- Ascending line
    ++ [ ((x + i, bottom - i), '/') | i <- [0 .. middleStop]]
        where (_, (ww, wh)) = sizeExtract whatSize
              (_, (ew, eh)) = sizeExtract endSize
              (_, (iw, _)) = sizeExtract iniSize
              sumWidth = w - 1 - ww
              whatBegin = x + w - 1 - ww
              bottom = y + eh + max 2 wh
              middleStop = wh `div` 2 + if wh `mod` 2 == 0
                    then -1 else 0

renderF (Matrix _n _m subs) (SizeNodeArray _ (_base,(w,h)) lst) (x,y) =
    renderSquareBracket (x,y) (w,h) True True
    ++ producedList final
     where renderLine :: (Int, Int) -> (Formula, (RelativePlacement, SizeTree))
                      -> ListProducer (Pos,Char) (Int,Int)
           renderLine (x', y') (formu, ((base,(w',_)),size)) = do
            let (nodeBase, (nodeWidth, _)) = sizeExtract size
                xStart = x' + (w' - nodeWidth) `div` 2
                yStart = y' + (base - nodeBase)
            concatFront $ renderF formu size (xStart, yStart)
            return (x' + w' + 1, y')
           
           renderMatrix (x', y') (formulas, sizes) = 
               let ((_,(_,height)),_) = head sizes
               in do
               foldM renderLine (x', y') $ zip formulas sizes
               return (x', y' + height + 1)

           final = foldM renderMatrix (x + 2, y + 1) $ zip subs lst

renderF _ _ _ = error "renderF - unmatched case"

