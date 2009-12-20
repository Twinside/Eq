module EqManips.Renderer.Placer( SizeTree( .. )
							   , Dimensioner( .. )
							   , Dimension, BaseLine, RelativePlacement
							   , sizeExtract 
							   , baseLineOfTree 
                               , sizeTreeOfFormula 
							   , sizeOfTree 
							   , maxPrio
							   ) where

import Data.List( foldl', transpose )
import Data.Ratio

import EqManips.Types
import EqManips.Polynome
import EqManips.Algorithm.Utils
import EqManips.Propreties
import EqManips.Renderer.RenderConf
import qualified EqManips.ErrorMessages as Err

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
    { unaryDim :: Conf -> UnOperator -> RelativePlacement -> RelativePlacement
    , varSize :: Conf -> String -> RelativePlacement
    , intSize :: Conf -> Integer -> RelativePlacement
    , floatSize :: Conf -> Double -> RelativePlacement
    , addParens :: Conf -> Dimension -> Dimension
    , remParens :: Conf -> Dimension -> Dimension
    , divBar :: Conf -> RelativePlacement -> RelativePlacement -> RelativePlacement
    , powSize :: Conf -> RelativePlacement -> RelativePlacement -> RelativePlacement
    , binop :: Conf -> BinOperator -> RelativePlacement -> RelativePlacement -> RelativePlacement
    , argSize :: Conf -> (Int, Int, Int) -> RelativePlacement -> (Int, Int, Int)
    , appSize :: Conf -> (Int, Int, Int) -> RelativePlacement -> RelativePlacement
    , lambdaSize :: Conf -> [((Int,Int,Int), RelativePlacement)] -> RelativePlacement
    , sumSize :: Conf -> RelativePlacement -> RelativePlacement -> RelativePlacement -> RelativePlacement
    , productSize :: Conf -> RelativePlacement -> RelativePlacement -> RelativePlacement -> RelativePlacement
    , integralSize :: Conf -> RelativePlacement -> RelativePlacement 
                   -> RelativePlacement -> RelativePlacement -> RelativePlacement
    , blockSize :: Conf -> (Int, Int, Int) -> RelativePlacement
    , matrixSize :: Conf -> [[RelativePlacement]] -> RelativePlacement
    , derivateSize :: Conf -> RelativePlacement -> RelativePlacement -> RelativePlacement
    , entitySize :: Conf -> Entity -> RelativePlacement
    , truthSize :: Conf -> Bool -> RelativePlacement
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
sizeTreeOfFormula :: Conf -> Dimensioner -> Formula TreeForm -> SizeTree
sizeTreeOfFormula conf dim (Formula a) = sizeOfFormula conf dim False maxPrio a

-- | Compute a size tree for a formula.
-- This size-tree can be used for a following render
sizeOfFormula :: Conf -> Dimensioner -> Bool -> OpPriority -> FormulaPrim -> SizeTree
-- INVISIBLE META NINJA
sizeOfFormula conf sizer a b (Meta _ f) = sizeOfFormula conf sizer a b f
-- Automatic conversion POLY NINJA
sizeOfFormula conf sizer a b (Fraction f) = 
    sizeOfFormula conf sizer a b
    $ (CInteger $ numerator f) / (CInteger $ denominator f)

sizeOfFormula conf sizer a b (Complex (re, im)) = 
    sizeOfFormula conf sizer a b $ re + Variable "i" * im
sizeOfFormula conf sizer a b (Poly p) =
    sizeOfFormula conf sizer a b . unTagFormula . treeIfyFormula $ convertToFormula p
-- Simply the size of rendered text
sizeOfFormula conf sizer _ _ (Variable v) = EndNode $ varSize sizer conf $ v
sizeOfFormula conf sizer _ _ (CInteger n) = EndNode $ intSize sizer conf $ n
sizeOfFormula conf sizer _ _ (CFloat f) = EndNode $ floatSize sizer conf $ f
sizeOfFormula conf sizer _ _ (Truth truthness) = EndNode $ truthSize sizer conf $ truthness
sizeOfFormula conf sizer _ _ (NumEntity f) = EndNode . entitySize sizer conf $ f
sizeOfFormula conf sizer _ _ (Block i1 i2 i3) = 
    EndNode $ (blockSize sizer) conf (i1, i2, i3)

-- Simply put a minus in front of the rest of the formula
sizeOfFormula conf sizer _ _ (UnOp op f) =
    MonoSizeNode False sizeDim subFormula
        where prio = op `obtainProp` Priority
              subFormula = sizeOfFormula conf sizer True prio f
              sizeDim = (unaryDim sizer) conf op (sizeExtract subFormula)

sizeOfFormula _ _ _ _ (BinOp _ [_]) = error $ Err.single_binop "sizeOfFormula conf - "
sizeOfFormula _ _ _ _ (BinOp _ []) = error $ Err.empty_binop "sizeOfFormula conf - "

-- do something like that :
--      ####
--     ------
--       #
--       #
sizeOfFormula conf sizer _ _ (BinOp OpDiv [f1,f2]) = 
  BiSizeNode False sizeDim nodeLeft nodeRight
    where nodeLeft = sizeOfFormula conf sizer False maxPrio f1
          nodeRight = sizeOfFormula conf sizer True maxPrio f2
          sizeDim = (divBar sizer) conf (sizeExtract nodeLeft) (sizeExtract nodeRight)

-- do something like that
--         %%%%%%%
--         %%%%%%%
--  #### ^ 
--  ####
sizeOfFormula conf sizer _isRight _prevPrio (BinOp OpPow [f1,f2]) =
  BiSizeNode False sizeDim nodeLeft nodeRight
    where nodeLeft = sizeOfFormula conf sizer False prioOfPow f1
          nodeRight = sizeOfFormula conf sizer True prioOfPow f2
          prioOfPow = OpPow `obtainProp` Priority
          sizeDim = (powSize sizer) conf (sizeExtract nodeLeft) (sizeExtract nodeRight)

-- add 3 char : ###### ! #######
-- we add spaces around operators
sizeOfFormula conf sizer isRight prevPrio (BinOp op [formula1, formula2]) =
  BiSizeNode needParenthes sizeDim nodeLeft nodeRight
    where prio = op `obtainProp` Priority
          needParenthes = needParenthesisPrio isRight prevPrio op

          nodeLeft = sizeOfFormula conf sizer False prio formula1
          nodeRight = sizeOfFormula conf sizer True prio formula2

          (base, s) = (binop sizer) conf op (sizeExtract nodeLeft) (sizeExtract nodeRight)

          sizeDim = if needParenthes
                then (base, addParens sizer conf s)
                else (base, s)

sizeOfFormula conf sizer r p f@(BinOp _ _) = 
    sizeOfFormula conf sizer r p $ treeIfyBinOp f

sizeOfFormula conf sizer _isRight _prevPrio (Integrate inite end what dx) =
    SizeNodeList False sizeDim 0 trees
        where sof = sizeOfFormula conf sizer False maxPrio
              trees = map sof [inite, end, what, dx]
              [iniDim, endDim, whatDim, dxDim] = map sizeExtract trees
              sizeDim = (integralSize sizer) conf iniDim endDim whatDim dxDim

sizeOfFormula conf sizer _ _ (Matrix _ _ exprs) =
    SizeNodeArray False sizeDim mixedMatrix
        where lineMapper = map (sizeOfFormula conf sizer False maxPrio)
              sizeMatrix = map lineMapper exprs

              sizeDim = matrixSize sizer conf dimensionMatrix

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

sizeOfFormula conf sizer _isRight _prevPrio (Product inite end what) =
    SizeNodeList False sizeDim 0 trees
        where sof = sizeOfFormula conf sizer False maxPrio
              trees = map sof [inite, end, what]
              [iniDim, endDim, whatDim] = map sizeExtract trees
              sizeDim = (productSize sizer) conf iniDim endDim whatDim


sizeOfFormula conf sizer _isRight _prevPrio (Derivate what vard) =
    BiSizeNode False sizeDim whatDim vardDim
        where whatDim = sizeOfFormula conf sizer False maxPrio what
              vardDim = sizeOfFormula conf sizer False maxPrio vard
              sizeDim = derivateSize sizer conf (sizeExtract whatDim)
                                           (sizeExtract vardDim)

sizeOfFormula conf sizer _isRight _prevPrio (Sum inite end what) =
    SizeNodeList False sizeDim 0 trees
        where sof = sizeOfFormula conf sizer False maxPrio
              trees = map sof [inite, end, what]
              [iniDim, endDim, whatDim] = map sizeExtract trees
              sizeDim = (sumSize sizer) conf iniDim endDim whatDim


-- do something like this :
--      #######
-- %%%% #######
-- %%%% #######
--      #######
sizeOfFormula conf sizer _ _ (App f1 f2) =
    SizeNodeList False sizeDim argsBase (funcSize : trees)
        where subSize = sizeOfFormula conf sizer False maxPrio
              trees = map subSize f2
              funcSize = subSize f1

              accumulated = argSizes sizer conf trees
              sizeDim = (appSize sizer) conf accumulated (sizeExtract funcSize)
              (_, argsBase, _) = accumulated

sizeOfFormula conf sizer _ _ (Lambda clauses) = SizeNodeClause False nodeSize finalTree
    where subSize = sizeOfFormula conf sizer False maxPrio 
          subTrees = [ (map subSize args, subSize body) | (args, body) <- clauses ]
          subPlacement = [(argSizes sizer conf args, sizeExtract body) | (args, body) <- subTrees]
          nodeSize = lambdaSize sizer conf subPlacement
          finalTree = [ (argBase, argTrees, bodyBase, bodyTree) 
                            | ( (argTrees, bodyTree)
                              , ((_, argBase,_),(bodyBase,_)) ) <- zip subTrees subPlacement]

argSizes :: Dimensioner -> Conf -> [SizeTree] -> (Int, Int, Int)
argSizes sizer conf args = foldl' sizeExtractor (0, 0, 0) args
    where sizeExtractor acc node = (argSize sizer) conf acc $ sizeExtract node

