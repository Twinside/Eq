{-# LANGUAGE ScopedTypeVariables #-}
module AsciiRenderer where

import Data.List( foldl' )
import Data.Array.Unboxed
import FormulaTypes( Formula( .. ), BinOperator( .. ), UnOperator( .. ))

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
    | SizeNodeList Bool BaseLine Dimension [SizeTree]
    deriving (Eq, Show, Read)

sizeOfTree :: SizeTree -> (Int, Int)
sizeOfTree (EndNode _ s) = s
sizeOfTree (MonoSizeNode _ _ s _) = s
sizeOfTree (BiSizeNode _ _ s _ _) = s
sizeOfTree (SizeNodeList _ _ s _) = s

baseLineOfTree :: SizeTree -> BaseLine
baseLineOfTree (EndNode b _) = b
baseLineOfTree (MonoSizeNode _ b _ _) = b
baseLineOfTree (BiSizeNode _ b _ _ _) = b
baseLineOfTree (SizeNodeList _ b _ _) = b

maxPrio, prioOfPow, prioOfMul, prioOfAdd :: Int
maxPrio = 100
prioOfPow = 1
prioOfMul = 2
prioOfAdd = 3

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
  BiSizeNode False (y1+1) (max x1 x2 + 2, y1 + y2 + 1) nodeLeft nodeRight
    where nodeLeft = sizeOfFormula False maxPrio f1
          nodeRight = sizeOfFormula True maxPrio f2
          (x1, y1) = sizeOfTree nodeLeft
          (x2, y2) = sizeOfTree nodeRight

sizeOfFormula _isRight _prevPrio (BinOp OpPow f1 f2) =
  BiSizeNode False base (x1 + x2 + 3, y1 + y2) nodeLeft nodeRight
    where nodeLeft = sizeOfFormula False maxPrio f1
          nodeRight = sizeOfFormula True maxPrio f2
          base = baseLineOfTree nodeLeft
          (x1, y1) = sizeOfTree nodeLeft
          (x2, y2) = sizeOfTree nodeRight

-- add 3 char : _+_
-- we add spaces around operators
sizeOfFormula isRight prevPrio (BinOp OpMul f1 f2) =
    combine isRight (3,0) prevPrio prioOfMul f1 f2
sizeOfFormula isRight prevPrio (BinOp OpAdd f1 f2) =
    combine isRight (3,0) prevPrio prioOfAdd f1 f2
sizeOfFormula isRight prevPrio (BinOp OpSub f1 f2) =
    combine isRight (3,0) prevPrio prioOfAdd f1 f2

-- BLA BLA BLA
sizeOfFormula _ _ (App f1 f2) =
    SizeNodeList False base finalSize (funcSize : trees)
        where trees = map (sizeOfFormula False maxPrio) f2
              sizes = map sizeOfTree trees
              funcSize = sizeOfFormula False maxPrio f1
              base = (snd $ sizeOfTree funcSize) `div` 2
              (xf, yf) = sizeOfTree funcSize
              (px, py) = foldl (\(xa, ya) (x,y) -> (xa + x + 2, max ya y)) 
                               (0, 0) sizes
              finalSize = (xf + px, max yf py)

-- | Helper function used to create a SizeTree
combine :: Bool -> Dimension -> Priority -> Priority -> Formula -> Formula -> SizeTree
combine isRight (xa, ya) prevPrio prio formula1 formula2 =
  ifPrio needParenthesis (x1 + x2 + xa, max y1 y2 + ya) nodeLeft nodeRight
    where ifPrio True size = BiSizeNode True base (addParens size)
          ifPrio False size = BiSizeNode False base size

          base = if y1 > y2
                    then baseLineOfTree nodeLeft
                    else baseLineOfTree nodeRight

          nodeLeft = sizeOfFormula False prio formula1
          nodeRight = sizeOfFormula True prio formula2

          (x1, y1) = sizeOfTree nodeLeft
          (x2, y2) = sizeOfTree nodeRight

          needParenthesis = if isRight then prio >= prevPrio
                                       else prio > prevPrio

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

renderBinaryOp :: Pos -> Char -> Formula -> Formula -> SizeTree -> [(Pos,Char)]
renderBinaryOp (x,y) op f1 f2 (BiSizeNode False _base (_,h) t1 t2) =
  ((x + lw + 1, y + h `div` 2),op) : (leftRender ++ rightRender)
    where (lw, lh) = sizeOfTree t1
          (_, rh) = sizeOfTree t2
          leftBaseline = y + (h - lh) `div` 2
          leftRender = renderF (x, leftBaseline) f1 t1

          rightBaseline = y + (h - rh) `div` 2
          rightRender = renderF (x + lw + 3, rightBaseline) f2 t2

renderBinaryOp _ _ _ _ _ = error "renderBinaryOp - wrong size tree node"

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

renderF (x,y) node (SizeNodeList True base dim stl) =
    renderParens (x,y) dim ++ renderF (x+1, y) node neoTree
        where subSize = remParens dim
              neoTree = SizeNodeList False base subSize stl

-- Here we make the "simple" rendering, just a conversion.
renderF (x,y) (Variable s) _ = map (\(idx,a) -> ((idx,y), a)) $ zip [x..] s
renderF (x,y) (CInteger i) _ = map (\(idx,a) -> ((idx,y), a)) $ zip [x..] (show i)
renderF (x,y) (CFloat d)   _ = map (\(idx,a) -> ((idx,y), a)) $ zip [x..] (show d)

-- basic binary operators, executed elsewhere
renderF pos (BinOp OpMul f1 f2) node = renderBinaryOp pos '*' f1 f2 node
renderF pos (BinOp OpAdd f1 f2) node = renderBinaryOp pos '+' f1 f2 node
renderF pos (BinOp OpSub f1 f2) node = renderBinaryOp pos '-' f1 f2 node

renderF (x,y) (BinOp OpPow f1 f2) (BiSizeNode False _base (_,_) t1 t2) =
    operator : leftRender ++ rightRender
    where operator = ((x + lw + 2, y + rh), '^')
          leftRender = renderF (x, y + rh) f1 t1
          rightRender = renderF (x + lw + 3, y) f2 t2
          (lw, _) = sizeOfTree t1
          (_, rh) = sizeOfTree t2

-- Division is of another kind :]
renderF (x,y) (BinOp OpDiv f1 f2) (BiSizeNode False _base (w,_) t1 t2) =
    [ ((xi,y + lh), '-') | xi <- [x .. x + w]] 
    ++ renderF (leftBegin , y) f1 t1 
    ++ renderF (rightBegin, y + lh + 1) f2 t2
        where (lw, lh) = sizeOfTree t1
              (rw, _) = sizeOfTree t2
              leftBegin = x + (w - lw) `div` 2
              rightBegin = x + (w - rw) `div` 2

renderF (x,y) (UnOp OpNegate f) (MonoSizeNode _ _ _ s) =
    ((x,y), '-') : renderF (x+1,y) f s

renderF (x,y) (App func flist) (SizeNodeList False _ (_,h) (s:ts)) =
    concat params
    ++ renderF (x,baseLine) func s
    ++ renderParens (x + fw, y) (xla - argBegin + 1, h)
        where (fw, _) = sizeOfTree s

              baseLine = y + h `div` 2

              mixedList = zip flist ts
              argBegin = x + fw + 1
              (params, (xla,_)) = foldl' write ([], (argBegin,y)) mixedList

              write (acc, (x',y')) (node, size) =
                  ( commas : argWrite : acc , (x' + nodeWidth + 2, y') )
                    where (nodeWidth, nodeHeight) = sizeOfTree size
                          commaBase = y + h `div` 2
                          commas = [((x' + nodeWidth + 1, commaBase), ',')] 
                          baseLine' = y' + (h - nodeHeight) `div` 2
                          argWrite = renderF (x', baseLine') node size 

{-renderF pos (OpPow f1 f2) (BiSizeNode False (w,h) t1 t2) =-}
renderF _ _ _ = error "renderF - unmatched case"
