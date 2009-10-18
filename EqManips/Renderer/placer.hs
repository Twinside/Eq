module EqManips.Renderer.Placer( SizeTree( .. )
							   , Dimensioner( .. )
							   , Dimension, BaseLine, RelativePlacement
							   , sizeExtract 
							   , baseLineOfTree 
                               , sizeTreeOfFormula 
							   , sizeOfTree 
							   , maxPrio
							   ) where

import qualified EqManips.ErrorMessages as Err
import EqManips.Types
import EqManips.Algorithm.Utils
import Data.List( foldl', transpose )
import EqManips.Propreties

type OpPriority = Int
type BaseLine = Int
type Dimension = (Int, Int)

type RelativePlacement = (BaseLine, Dimension)

-- | Size tree used to store the block size to
-- render the equation in ASCII
data SizeTree =
      EndNode        RelativePlacement
    | MonoSizeNode   Bool RelativePlacement SizeTree
    | BiSizeNode     Bool RelativePlacement SizeTree   SizeTree
    | SizeNodeList   Bool RelativePlacement BaseLine   [SizeTree]
    | SizeNodeClause Bool RelativePlacement [(BaseLine, [SizeTree], BaseLine, SizeTree)]
    | SizeNodeArray  Bool RelativePlacement [[(RelativePlacement, SizeTree)]]
    deriving (Eq, Show)

-- | an "object" which is used to get the placement of all the elements in the equation.
data Dimensioner = Dimensioner
    { unaryDim :: UnOperator -> RelativePlacement -> RelativePlacement
    , varSize :: String -> RelativePlacement
    , intSize :: Int -> RelativePlacement
    , floatSize :: Double -> RelativePlacement
    , addParens :: Dimension -> Dimension
    , remParens :: Dimension -> Dimension
    , divBar :: RelativePlacement -> RelativePlacement -> RelativePlacement
    , powSize :: RelativePlacement -> RelativePlacement -> RelativePlacement
    , binop :: BinOperator -> RelativePlacement -> RelativePlacement -> RelativePlacement
    , argSize :: (Int, Int, Int) -> RelativePlacement -> (Int, Int, Int)
    , appSize :: (Int, Int, Int) -> RelativePlacement -> RelativePlacement
    , lambdaSize :: [((Int,Int,Int), RelativePlacement)] -> RelativePlacement
    , sumSize :: RelativePlacement -> RelativePlacement -> RelativePlacement -> RelativePlacement
    , productSize :: RelativePlacement -> RelativePlacement -> RelativePlacement -> RelativePlacement
    , integralSize :: RelativePlacement -> RelativePlacement 
                   -> RelativePlacement -> RelativePlacement -> RelativePlacement
    , blockSize :: (Int, Int, Int) -> RelativePlacement
    , matrixSize :: [[RelativePlacement]] -> RelativePlacement
    , derivateSize :: RelativePlacement -> RelativePlacement -> RelativePlacement
    , entitySize :: Entity -> RelativePlacement
    , truthSize :: Bool -> RelativePlacement
    }

sizeExtract :: SizeTree -> RelativePlacement
sizeExtract (EndNode s) = s
sizeExtract (MonoSizeNode _ s _) = s
sizeExtract (BiSizeNode _ s _ _) = s
sizeExtract (SizeNodeList _ s _ _) = s
sizeExtract (SizeNodeArray _ s _) = s
sizeExtract (SizeNodeClause _ s _) = s

sizeOfTree :: SizeTree -> (Int, Int)
sizeOfTree = snd . sizeExtract

baseLineOfTree :: SizeTree -> BaseLine
baseLineOfTree = fst . sizeExtract

maxPrio :: Int
maxPrio = 100

-- | Obtain a size tree for a formula given
-- an desired outputter.
sizeTreeOfFormula :: Dimensioner -> Formula -> SizeTree
sizeTreeOfFormula dim = sizeOfFormula dim False maxPrio

-- | Compute a size tree for a formula.
-- This size-tree can be used for a following render
sizeOfFormula :: Dimensioner -> Bool -> OpPriority -> Formula -> SizeTree
-- INVISIBLE META NINJA
sizeOfFormula sizer a b (Meta _ f) = sizeOfFormula sizer a b f
-- Simply the size of rendered text
sizeOfFormula sizer _ _ (Variable v) = EndNode $ varSize sizer $ v
sizeOfFormula sizer _ _ (CInteger n) = EndNode $ intSize sizer $ n
sizeOfFormula sizer _ _ (CFloat f) = EndNode $ floatSize sizer $ f
sizeOfFormula sizer _ _ (Truth truthness) = EndNode $ truthSize sizer $ truthness
sizeOfFormula sizer _ _ (NumEntity f) = EndNode . entitySize sizer $ f
sizeOfFormula sizer _ _ (Block i1 i2 i3) = 
    EndNode $ (blockSize sizer) (i1, i2, i3)

-- Simply put a minus in front of the rest of the formula
sizeOfFormula sizer _ _ (UnOp op f) =
    MonoSizeNode False sizeDim subFormula
        where prio = op `obtainProp` Priority
              subFormula = sizeOfFormula sizer True prio f
              sizeDim = (unaryDim sizer) op (sizeExtract subFormula)

sizeOfFormula _ _ _ (BinOp _ [_]) = error $ Err.single_binop "sizeOfFormula - "
sizeOfFormula _ _ _ (BinOp _ []) = error $ Err.empty_binop "sizeOfFormula - "

-- do something like that :
--      ####
--     ------
--       #
--       #
sizeOfFormula sizer _ _ (BinOp OpDiv [f1,f2]) = 
  BiSizeNode False sizeDim nodeLeft nodeRight
    where nodeLeft = sizeOfFormula sizer False maxPrio f1
          nodeRight = sizeOfFormula sizer True maxPrio f2
          sizeDim = (divBar sizer) (sizeExtract nodeLeft) (sizeExtract nodeRight)

-- do something like that
--         %%%%%%%
--         %%%%%%%
--  #### ^ 
--  ####
sizeOfFormula sizer _isRight _prevPrio (BinOp OpPow [f1,f2]) =
  BiSizeNode False sizeDim nodeLeft nodeRight
    where nodeLeft = sizeOfFormula sizer False prioOfPow f1
          nodeRight = sizeOfFormula sizer True prioOfPow f2
          prioOfPow = OpPow `obtainProp` Priority
          sizeDim = (powSize sizer) (sizeExtract nodeLeft) (sizeExtract nodeRight)

-- add 3 char : ###### ! #######
-- we add spaces around operators
sizeOfFormula sizer isRight prevPrio (BinOp op [formula1, formula2]) =
  BiSizeNode needParenthes sizeDim nodeLeft nodeRight
    where prio = op `obtainProp` Priority
          needParenthes = needParenthesisPrio isRight prevPrio op

          nodeLeft = sizeOfFormula sizer False prio formula1
          nodeRight = sizeOfFormula sizer True prio formula2

          (base, s) = (binop sizer) op (sizeExtract nodeLeft) (sizeExtract nodeRight)

          sizeDim = if needParenthes
                then (base, addParens sizer s)
                else (base, s)

sizeOfFormula sizer r p f@(BinOp _ _) = 
    sizeOfFormula sizer r p $ treeIfyBinOp f

sizeOfFormula sizer _isRight _prevPrio (Integrate inite end what dx) =
    SizeNodeList False sizeDim 0 trees
        where sof = sizeOfFormula sizer False maxPrio
              trees = map sof [inite, end, what, dx]
              [iniDim, endDim, whatDim, dxDim] = map sizeExtract trees
              sizeDim = (integralSize sizer) iniDim endDim whatDim dxDim

sizeOfFormula sizer _ _ (Matrix _ _ exprs) =
    SizeNodeArray False sizeDim mixedMatrix
        where lineMapper = map (sizeOfFormula sizer False maxPrio)
              sizeMatrix = map lineMapper exprs

              sizeDim = matrixSize sizer dimensionMatrix

              baseLineExtractor :: (Int, Int) -> SizeTree -> (Int,Int)
              baseLineExtractor (base, depth) size =
                  let (base', (_,h')) = sizeExtract size
                  in (max base base', max depth (h' - base'))

              heights :: [(Int,Int)]
              heights = map (foldl' baseLineExtractor (0,0)) sizeMatrix

              widths :: [Int]
              widths =
                   [ maximum $ map widthOf column | column <- transpose sizeMatrix ]

              widthOf :: SizeTree -> Int
              widthOf size = fst . snd $ sizeExtract size

              dimensionMatrix =
                  [ [(bases, (w, bases + depth)) | w <- widths] 
                        | (bases, depth) <- heights]

              mixedMatrix =
                  [ zip dims sizes
                    | (dims, sizes) <- zip dimensionMatrix sizeMatrix]

sizeOfFormula sizer _isRight _prevPrio (Product inite end what) =
    SizeNodeList False sizeDim 0 trees
        where sof = sizeOfFormula sizer False maxPrio
              trees = map sof [inite, end, what]
              [iniDim, endDim, whatDim] = map sizeExtract trees
              sizeDim = (productSize sizer) iniDim endDim whatDim


sizeOfFormula sizer _isRight _prevPrio (Derivate what vard) =
    BiSizeNode False sizeDim whatDim vardDim
        where whatDim = sizeOfFormula sizer False maxPrio what
              vardDim = sizeOfFormula sizer False maxPrio vard
              sizeDim = derivateSize sizer (sizeExtract whatDim)
                                           (sizeExtract vardDim)

sizeOfFormula sizer _isRight _prevPrio (Sum inite end what) =
    SizeNodeList False sizeDim 0 trees
        where sof = sizeOfFormula sizer False maxPrio
              trees = map sof [inite, end, what]
              [iniDim, endDim, whatDim] = map sizeExtract trees
              sizeDim = (sumSize sizer) iniDim endDim whatDim


-- do something like this :
--      #######
-- %%%% #######
-- %%%% #######
--      #######
sizeOfFormula sizer _ _ (App f1 f2) =
    SizeNodeList False sizeDim argsBase (funcSize : trees)
        where subSize = sizeOfFormula sizer False maxPrio
              trees = map subSize f2
              funcSize = subSize f1

              accumulated = argSizes sizer trees
              sizeDim = (appSize sizer) accumulated (sizeExtract funcSize)
              (_, argsBase, _) = accumulated

sizeOfFormula sizer _ _ (Lambda clauses) = SizeNodeClause False nodeSize finalTree
    where subSize = sizeOfFormula sizer False maxPrio 
          subTrees = [ (map subSize args, subSize body) | (args, body) <- clauses ]
          subPlacement = [(argSizes sizer args, sizeExtract body) | (args, body) <- subTrees]
          nodeSize = lambdaSize sizer subPlacement
          finalTree = [ (argBase, argTrees, bodyBase, bodyTree) 
                            | ( (argTrees, bodyTree)
                              , ((_, argBase,_),(bodyBase,_)) ) <- zip subTrees subPlacement]

argSizes :: Dimensioner -> [SizeTree] -> (Int, Int, Int)
argSizes sizer args = foldl' sizeExtractor (0, 0, 0) args
    where sizeExtractor acc node = (argSize sizer) acc $ sizeExtract node

