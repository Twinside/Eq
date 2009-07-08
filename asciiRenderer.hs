{-# LANGUAGE ScopedTypeVariables #-}
module AsciiRenderer where

import Data.List( foldl' )
import Data.Array.Unboxed
import FormulaTypes
import FormulaPlacer

type Pos = (Int, Int)

asciiSizer :: Dimensioner
asciiSizer = Dimensioner
    { unaryDim = \op (base, (w,h)) ->
        let s OpNegate = (base, (w + 1, h))
            s OpAbs = (base, (w + 2, h))
            s OpSqrt = (base, (w + (h * 3) `div` 2, h + 1))
            {-s _ = error "EEEEEEEERgl"-}
        in s op

    , varSize = \s -> (0, (length s, 1))
    , intSize = \i -> (0, (length $ show i,1))
    , floatSize = \f -> (0, (length $ show f, 1))
    , addParens = \(w, h) -> (w + 2, h)
    , remParens = \(w, h) -> (w - 2, h)
    , divBar = \(_,(w1,h1)) (_,(w2,h2)) ->
                    (h1, (max w1 w2 + 2, h1 + h2 + 1))
    , powSize = \(b,(w1,h1)) (_,(w2,h2)) ->
                    (b + h2, (w1 + w2 + 3, h1 + h2))

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
    }

-------------------------------------------------------------
----                     Rendering                       ----
-------------------------------------------------------------
renderFormula :: Formula -- ^ Formula to render
              -> (UArray (Int,Int) Char,SizeTree) -- ^ Rendered formula
renderFormula formula = 
    (accumArray (flip const) ' ' size writeList, sizeTree)
        where sizeTree = sizeOfFormula asciiSizer False maxPrio formula
              size = ((0,0), sizeOfTree sizeTree)
              writeList = renderF (0,0) formula sizeTree

-- | One function to render them all! (parenthesis)
-- for one line ( ... )
-- else we try to render something like that :
-- /        \
-- |        |
-- |        |
-- \        /
renderParens :: Pos -> Dimension -> [(Pos, Char)]
renderParens (x,y) (w,1) = [((x,y), '('), ((x + w - 1, y), ')')]
renderParens (x,y) (w,h) =
    ((x       , y ), '/' ) : ((x       , lastLine), '\\') :
    ((rightCol, y ), '\\') : ((rightCol, lastLine), '/' ) :
    concat [ [ ((rightCol, height), '|')
             , ((x       , height), '|')] | height <- [y+1 .. lastLine - 1] ]
       where rightCol = x + w - 1
             lastLine = y + h - 1


charOfOp :: BinOperator -> Char
charOfOp OpAdd = '+'
charOfOp OpSub = '-'
charOfOp OpMul = '*'
charOfOp OpDiv = '/'
charOfOp OpPow = '^'

renderF :: Pos -- ^ Where to render
        -> Formula -- ^ CurrentNode
        -> SizeTree -- ^ Previously calculated size
        -> [(Pos,Char)] -- ^ Result to be used in accumArray

-- In the following matches, we render parenthesis and
-- then recurse to the normal flow for the regular render.
renderF (x,y) node (MonoSizeNode True (base, dim) st) =
    renderParens (x,y) dim ++ renderF (x+1, y) node neoTree
        where subSize = (remParens asciiSizer) dim
              neoTree = MonoSizeNode False (base, subSize) st
renderF (x,y) node (BiSizeNode True (base, dim) st1 st2) =
    renderParens (x,y) dim ++ renderF (x+1, y) node neoTree
        where subSize = (remParens asciiSizer) dim
              neoTree = BiSizeNode False (base, subSize) st1 st2

renderF (x,y) node (SizeNodeList True (base, dim) abase stl) =
    renderParens (x,y) dim ++ renderF (x+1, y) node neoTree
        where subSize = (remParens asciiSizer) dim
              neoTree = SizeNodeList False (base, subSize) abase stl

-- Here we make the "simple" rendering, just a conversion.
renderF (x,y) (Variable s) _ = map (\(idx,a) -> ((idx,y), a)) $ zip [x..] s
renderF (x,y) (CInteger i) _ = map (\(idx,a) -> ((idx,y), a)) $ zip [x..] (show i)
renderF (x,y) (CFloat d)   _ = map (\(idx,a) -> ((idx,y), a)) $ zip [x..] (show d)

renderF (x,y) (BinOp OpPow f1 f2) (BiSizeNode False _ t1 t2) =
    operator : leftRender ++ rightRender
    where operator = ((x + lw + 2, y + rh), '^')
          leftRender = renderF (x, y + rh) f1 t1
          rightRender = renderF (x + lw + 3, y) f2 t2
          (lw, _) = sizeOfTree t1
          (_, rh) = sizeOfTree t2

-- Division is of another kind :]
renderF (x,y) (BinOp OpDiv f1 f2) (BiSizeNode False (_,(w,_)) t1 t2) =
    [ ((xi,y + lh), '-') | xi <- [x .. x + w - 1]] 
    ++ renderF (leftBegin , y) f1 t1 
    ++ renderF (rightBegin, y + lh + 1) f2 t2
        where (lw, lh) = sizeOfTree t1
              (rw, _) = sizeOfTree t2
              leftBegin = x + (w - lw) `div` 2
              rightBegin = x + (w - rw) `div` 2

renderF (x,y) (BinOp op f1 f2) (BiSizeNode False (base,_) t1 t2) =
  ((x + lw + 1, y + base),opChar) : (leftRender ++ rightRender)
    where (lw, _) = sizeOfTree t1
          leftBase = baseLineOfTree t1
          rightBase = baseLineOfTree t2
          opChar = charOfOp op

          (leftTop, rightTop) =
              if leftBase > rightBase
                 then (y, y + leftBase - rightBase)
                 else (y + rightBase - leftBase, y)

          leftRender = renderF (x, leftTop) f1 t1
          rightRender = renderF (x + lw + 3, rightTop) f2 t2

renderF (x,y) (UnOp OpSqrt f) (MonoSizeNode _ (_,(w,h)) s) =
    -- tail of root
    ((x + h `div` 2, y + h), 'v') :
    -- The sub formula
    renderF (leftBegin - 1, y + 1) f s
    -- The top line
    ++ [ ((left,y), '_') | left <- [leftBegin .. x + w] ]
    -- big line from bottom to top
    ++ [ ((x + h `div` 2 + i, y + h - i), '/') | i <- [1 .. h - 1] ]
    -- Tiny line from middle to bottom
    ++ [ ((x + i, h `div` 2 + i), '\\') | i <- [0 .. h `div` 2 - 1]]
        where leftBegin = x + (h * 3) `div` 2

renderF (x,y) (UnOp OpNegate f) (MonoSizeNode _ _ s) =
    ((x,y), '-') : renderF (x+1,y) f s

renderF (x,y) (UnOp OpAbs f) (MonoSizeNode _ (_,(w,h)) s) =
    concat [  [((x,height), '|'), ((x + w - 1, height), '|')]
             | height <- [y .. y + h - 1] ]
    ++ renderF (x+1,y) f s

renderF (x,y) (App func flist) (SizeNodeList False (base, (_,h)) argBase (s:ts)) =
    concat params
    ++ renderF (x,baseLine) func s
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
                          argWrite = renderF (x', baseLine') node size 

renderF _ _ _ = error "renderF - unmatched case"
