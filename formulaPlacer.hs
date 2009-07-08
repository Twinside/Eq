module FormulaPlacer where

import FormulaTypes
import Data.List( foldl' )

type Priority = Int
type BaseLine = Int
type Dimension = (Int, Int)

-- | Size tree used to store the block size to
-- render the equation in ASCII
data SizeTree =
      EndNode (BaseLine, Dimension)
    | MonoSizeNode Bool (BaseLine, Dimension) SizeTree
    | BiSizeNode Bool (BaseLine, Dimension) SizeTree SizeTree
    | SizeNodeList Bool (BaseLine, Dimension) BaseLine [SizeTree]
    deriving (Eq, Show, Read)

type RelativePlacement = (BaseLine, Dimension)

data Dimensioner = Dimensioner
    { unaryDim :: UnOperator -> Dimension -> RelativePlacement
    , varSize :: String -> RelativePlacement
    , intSize :: Int -> RelativePlacement
    , floatSize :: Double -> RelativePlacement
    , addParens :: Dimension -> Dimension
    , remParens :: Dimension -> Dimension
    , divBar :: RelativePlacement -> RelativePlacement -> RelativePlacement
    , powSize :: RelativePlacement -> RelativePlacement -> RelativePlacement
    , binop :: RelativePlacement -> RelativePlacement -> RelativePlacement

    , argSize :: (Int, Int, Int) -> RelativePlacement -> (Int, Int, Int)
    , appSize :: (Int,Int,Int) -> RelativePlacement -> RelativePlacement
    }

sizeExtract :: SizeTree -> (BaseLine, Dimension)
sizeExtract (EndNode s) = s
sizeExtract (MonoSizeNode _ s _) = s
sizeExtract (BiSizeNode _ s _ _) = s
sizeExtract (SizeNodeList _ s _ _) = s

sizeOfTree :: SizeTree -> (Int, Int)
sizeOfTree = snd . sizeExtract

baseLineOfTree :: SizeTree -> BaseLine
baseLineOfTree = fst . sizeExtract

maxPrio :: Int
maxPrio = 100

-- | Compute a size tree for a formula.
-- This size-tree can be used for a following render
sizeOfFormula :: Dimensioner -> Bool -> Priority -> Formula -> SizeTree
-- Simply the size of rendered text
sizeOfFormula sizer _ _ (Variable v) = EndNode $ varSize sizer $ v
sizeOfFormula sizer _ _ (CInteger n) = EndNode $ intSize sizer $ n
sizeOfFormula sizer _ _ (CFloat f) = EndNode $ floatSize sizer $ f

-- Simply put a minus in front of the rest of the formula
sizeOfFormula sizer _ _ (UnOp OpNegate f) =
    MonoSizeNode False (base, (xa + x, ya + y)) subFormula
        where subFormula = sizeOfFormula sizer False maxPrio f
              subSize = sizeOfTree subFormula
              base = (snd subSize) `div` 2
              (xa, ya) = snd $ (unaryDim sizer) OpNegate subSize
              (x, y) = sizeOfTree subFormula


-- do something like that :
--      ####
--     ------
--       #
--       #
sizeOfFormula sizer _ _ (BinOp OpDiv f1 f2) = 
  BiSizeNode False sizeDim nodeLeft nodeRight
    where nodeLeft = sizeOfFormula sizer False maxPrio f1
          nodeRight = sizeOfFormula sizer True maxPrio f2
          sizeDim = (divBar sizer) (sizeExtract nodeLeft) (sizeExtract nodeRight)

-- do something like that
--         %%%%%%%
--         %%%%%%%
--  #### ^ 
--  ####
sizeOfFormula sizer _isRight _prevPrio (BinOp OpPow f1 f2) =
  BiSizeNode False sizeDim nodeLeft nodeRight
    where nodeLeft = sizeOfFormula sizer False prioOfPow f1
          nodeRight = sizeOfFormula sizer True prioOfPow f2
          prioOfPow = prioOfBinaryOperators OpPow
          sizeDim = (powSize sizer) (sizeExtract nodeLeft) (sizeExtract nodeRight)

-- add 3 char : ###### ! #######
-- we add spaces around operators
sizeOfFormula sizer isRight prevPrio (BinOp op formula1 formula2) =
  BiSizeNode needParenthesis sizeDim nodeLeft nodeRight
    where prio = prioOfBinaryOperators op

          needParenthesis = if isRight then prio >= prevPrio
                                       else prio > prevPrio

          nodeLeft = sizeOfFormula sizer False prio formula1
          nodeRight = sizeOfFormula sizer True prio formula2

          (base, s) = (binop sizer) (sizeExtract nodeLeft) (sizeExtract nodeRight)

          sizeDim = if needParenthesis
                then (base, addParens sizer s)
                else (base, s)

-- do something like this :
--      #######
-- %%%% #######
-- %%%% #######
--      #######
sizeOfFormula sizer _ _ (App f1 f2) =
    SizeNodeList False sizeDim argsBase (funcSize : trees)
        where trees = map (sizeOfFormula sizer False maxPrio) f2
              funcSize = sizeOfFormula sizer False maxPrio f1

              sizeExtractor acc node =
                  (argSize sizer) acc $ sizeExtract node

              sizeDim = (appSize sizer) accumulated (sizeExtract funcSize)
              accumulated = foldl' sizeExtractor (0, 0, 0) trees
              (_, argsBase, _) = accumulated

