{-# LANGUAGE ScopedTypeVariables #-}
module AsciiRenderer where

import Data.List( foldl' )
import Data.Array.Unboxed
import FormulaTypes

type Priority = Int

type Pos = (Int, Int)
type Dimension = (Int, Int)
type BaseLine = Int

-- | Size tree used to store the block size to
-- render the equation in ASCII
data SizeTree =
      EndNode BaseLine Dimension
    | MonoSizeNode Bool BaseLine Dimension SizeTree
    | BiSizeNode Bool BaseLine Dimension SizeTree SizeTree
    | SizeNodeList Bool BaseLine Dimension BaseLine [SizeTree]
    deriving (Eq, Show, Read)

sizeOfTree :: SizeTree -> (Int, Int)
sizeOfTree (EndNode _ s) = s
sizeOfTree (MonoSizeNode _ _ s _) = s
sizeOfTree (BiSizeNode _ _ s _ _) = s
sizeOfTree (SizeNodeList _ _ s _ _) = s

baseLineOfTree :: SizeTree -> BaseLine
baseLineOfTree (EndNode b _) = b
baseLineOfTree (MonoSizeNode _ b _ _) = b
baseLineOfTree (BiSizeNode _ b _ _ _) = b
baseLineOfTree (SizeNodeList _ b _ _ _) = b

maxPrio :: Int
maxPrio = 100

-- | Helper function to create a monoNode
monoNode :: BaseLine -> Dimension -> SizeTree -> SizeTree
monoNode base (xa, ya) subFormula =
    MonoSizeNode False base (xa + x, ya + y) subFormula
        where (x, y) = sizeOfTree subFormula

addParens :: Dimension -> Dimension
addParens (x, y) = (x + 2, y)

remParens :: Dimension -> Dimension
remParens (x, y) = (x + 2, y)

-- | Compute a size tree for a formula.
-- This size-tree can be used for a following render
sizeOfFormula :: Bool -> Priority -> Formula -> SizeTree
-- Simply the size of rendered text
sizeOfFormula _ _ (Variable v) = EndNode 0 (length v, 1)
sizeOfFormula _ _ (CInteger n) = EndNode 0 (length $ show n,1)
sizeOfFormula _ _ (CFloat f) = EndNode 0 (length $ show f, 1)

-- Simply put a minus in front of the rest of the formula
sizeOfFormula _ _ (UnOp OpNegate f) =
    monoNode base (1, 0) size
        where size = sizeOfFormula False maxPrio f
              base = (snd $ sizeOfTree size) `div` 2


-- do something like that :
--      ####
--     ------
--       #
--       #
sizeOfFormula _ _ (BinOp OpDiv f1 f2) = 
  BiSizeNode False base (max x1 x2 + 2, y1 + y2 + 1) nodeLeft nodeRight
    where nodeLeft = sizeOfFormula False maxPrio f1
          nodeRight = sizeOfFormula True maxPrio f2
          base = y1
          (x1, y1) = sizeOfTree nodeLeft
          (x2, y2) = sizeOfTree nodeRight

-- do something like that
--         %%%%%%%
--         %%%%%%%
--  #### ^ 
--  ####
sizeOfFormula _isRight _prevPrio (BinOp OpPow f1 f2) =
  BiSizeNode False base (x1 + x2 + 3, y1 + y2) nodeLeft nodeRight
    where nodeLeft = sizeOfFormula False prioOfPow f1
          nodeRight = sizeOfFormula True prioOfPow f2
          base = baseLineOfTree nodeLeft + y2
          (x1, y1) = sizeOfTree nodeLeft
          (x2, y2) = sizeOfTree nodeRight
          prioOfPow = prioOfBinaryOperators OpPow

-- add 3 char : ###### ! #######
-- we add spaces around operators
sizeOfFormula isRight prevPrio (BinOp op formula1 formula2) =
  ifPrio needParenthesis (x1 + x2 + xa, nodeSize) nodeLeft nodeRight
    where ifPrio True size = BiSizeNode True base (addParens size)
          ifPrio False size = BiSizeNode False base size

          prio = prioOfBinaryOperators op
          (xa, ya) = (3, 0)
          baseLeft = baseLineOfTree nodeLeft
          baseRight = baseLineOfTree nodeRight

          nodeLeft = sizeOfFormula False prio formula1
          nodeRight = sizeOfFormula True prio formula2

          (x1, y1) = sizeOfTree nodeLeft
          (x2, y2) = sizeOfTree nodeRight

          -- We must handle case like this :
          --  +-------+
          --  |       |+-------+
          --  +-------|+-------+
          --  |       ||       |
          --  +-------+|       |
          --           +-------+
          base = max baseLeft baseRight
          nodeSize = base + ya + max (y1 - baseLeft) (y2 - baseRight)

          needParenthesis = if isRight then prio >= prevPrio
                                       else prio > prevPrio

-- do something like this :
--      #######
-- %%%% #######
-- %%%% #######
--      #######
sizeOfFormula _ _ (App f1 f2) =
    SizeNodeList False base finalSize argsBase (funcSize : trees)
        where trees = map (sizeOfFormula False maxPrio) f2

              funcSize = sizeOfFormula False maxPrio f1

              sizeExtractor (xa, argBase, lower) node =
                  (xa + x + 2, max argBase nodeBase, max lower (y-nodeBase))
                    where (x,y) = sizeOfTree node
                          nodeBase = baseLineOfTree node

              (xf, yf) = sizeOfTree funcSize
              (px, argsBase, argsLeft) = foldl' sizeExtractor (0, 0, 0) trees
              finalY = max yf (argsBase + argsLeft)
              base = (finalY - yf) `div` 2

              finalSize = (xf + px + 1, finalY)

-------------------------------------------------------------
----                     Rendering                       ----
-------------------------------------------------------------
renderFormula :: Formula -- ^ Formula to render
              -> (UArray (Int,Int) Char,SizeTree) -- ^ Rendered formula
renderFormula formula = 
    (accumArray (flip const) ' ' size writeList, sizeTree)
        where sizeTree = sizeOfFormula False maxPrio formula
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
renderF (x,y) node (MonoSizeNode True base dim st) =
    renderParens (x,y) dim ++ renderF (x+1, y) node neoTree
        where subSize = remParens dim
              neoTree = MonoSizeNode False base subSize st
renderF (x,y) node (BiSizeNode True base dim st1 st2) =
    renderParens (x,y) dim ++ renderF (x+1, y) node neoTree
        where subSize = remParens dim
              neoTree = BiSizeNode False base subSize st1 st2

renderF (x,y) node (SizeNodeList True base dim abase stl) =
    renderParens (x,y) dim ++ renderF (x+1, y) node neoTree
        where subSize = remParens dim
              neoTree = SizeNodeList False base subSize abase stl

-- Here we make the "simple" rendering, just a conversion.
renderF (x,y) (Variable s) _ = map (\(idx,a) -> ((idx,y), a)) $ zip [x..] s
renderF (x,y) (CInteger i) _ = map (\(idx,a) -> ((idx,y), a)) $ zip [x..] (show i)
renderF (x,y) (CFloat d)   _ = map (\(idx,a) -> ((idx,y), a)) $ zip [x..] (show d)

renderF (x,y) (BinOp OpPow f1 f2) (BiSizeNode False _base (_,_) t1 t2) =
    operator : leftRender ++ rightRender
    where operator = ((x + lw + 2, y + rh), '^')
          leftRender = renderF (x, y + rh) f1 t1
          rightRender = renderF (x + lw + 3, y) f2 t2
          (lw, _) = sizeOfTree t1
          (_, rh) = sizeOfTree t2

-- Division is of another kind :]
renderF (x,y) (BinOp OpDiv f1 f2) (BiSizeNode False _base (w,_) t1 t2) =
    [ ((xi,y + lh), '-') | xi <- [x .. x + w - 1]] 
    ++ renderF (leftBegin , y) f1 t1 
    ++ renderF (rightBegin, y + lh + 1) f2 t2
        where (lw, lh) = sizeOfTree t1
              (rw, _) = sizeOfTree t2
              leftBegin = x + (w - lw) `div` 2
              rightBegin = x + (w - rw) `div` 2

renderF (x,y) (BinOp op f1 f2) (BiSizeNode False _ _ t1 t2) =
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

renderF (x,y) (UnOp OpNegate f) (MonoSizeNode _ _ _ s) =
    ((x,y), '-') : renderF (x+1,y) f s

renderF (x,y) (App func flist) (SizeNodeList False base (_,h) argBase (s:ts)) =
    concat params
    ++ renderF (x,baseLine) func s
    ++ renderParens (x + fw, y) (xla - argBegin + 1, h)
        where (fw, _) = sizeOfTree s

              baseLine = y + base

              mixedList = zip flist ts
              argBegin = x + fw + 1
              (params, (xla,_)) = foldl' write ([], (argBegin,y)) mixedList

              write (acc, (x',y')) (node, size) =
                  ( commas : argWrite : acc , (x' + nodeWidth + 2, y') )
                    where (nodeWidth, nodeHeight) = sizeOfTree size
                          commas = [((x' + nodeWidth + 1, argBase), ',')] 
                          nodeBase = baseLineOfTree size
                          baseLine' = y' + (argBase - nodeBase)
                          argWrite = renderF (x', baseLine') node size 

renderF _ _ _ = error "renderF - unmatched case"
