{-# LANGUAGE ScopedTypeVariables #-}
module AsciiRenderer where

import Data.List( foldl' )
import Data.Array.Unboxed
import FormulaTypes
import FormulaPlacer

type Pos = (Int, Int)

asciiSizer :: Dimensioner
asciiSizer = Dimensioner
    { unaryDim = \_ _ -> (0, (1,0))
    , varSize = \s -> (0, (length s, 1))
    , intSize = \i -> (0, (length $ show i,1))
    , floatSize = \f -> (0, (length $ show f, 1))
    , addParens = \(x, y) -> (x + 2, y)
    , remParens = \(x, y) -> (x - 2, y)
    , divBar = \(_,(x1,y1)) (_,(x2,y2)) ->
                    (y1, (max x1 x2 + 2, y1 + y2 + 1))
    , powSize = \(b,(x1,y1)) (_,(x2,y2)) ->
                    (b, (x1 + x2 + 3, y1 + y2))

      -- We must handle case like this :
      --  +-------+
      --  |       |+-------+
      --  +-------|+-------+
      --  |       ||       |
      --  +-------+|       |
      --           +-------+
    , binop = \(bl,(x1,y1)) (br,(x2,y2)) ->
                    let base = max bl br
                        nodeSize = base + max (y1 - bl) (y2 - br)
                    in (base, (x1 + x2 + 3, nodeSize))

    , argSize = \(xa, argBase, lower) (nodeBase, (x,y)) ->
                  (xa + x + 2, max argBase nodeBase, max lower (y-nodeBase))

    , appSize = \(px, argsBase, argsLeft) (_, (xf, yf)) ->
            let finalY = max yf (argsBase + argsLeft)
            in ((finalY - yf) `div` 2, (xf + px, finalY))
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

renderF (x,y) (BinOp op f1 f2) (BiSizeNode False _ t1 t2) =
  ((x + lw + 1, base),opChar) : (leftRender ++ rightRender)
    where (lw, _) = sizeOfTree t1
          (_, _) = sizeOfTree t2
          leftBase = baseLineOfTree t1
          rightBase = baseLineOfTree t2
          opChar = charOfOp op

          (leftTop, rightTop, base) =
              if leftBase > rightBase
                 then (y, y + leftBase - rightBase, y + leftBase)
                 else (y + rightBase - leftBase, y, y + rightBase)

          leftRender = renderF (x, leftTop) f1 t1
          rightRender = renderF (x + lw + 3, rightTop) f2 t2

renderF (x,y) (UnOp OpNegate f) (MonoSizeNode _ _ s) =
    ((x,y), '-') : renderF (x+1,y) f s

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
                          commas = [((x' + nodeWidth, argBase), ',')] 
                          nodeBase = baseLineOfTree size
                          baseLine' = y' + (argBase - nodeBase)
                          argWrite = renderF (x', baseLine') node size 

renderF _ _ _ = error "renderF - unmatched case"
