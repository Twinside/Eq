module EqManips.Renderer.Ascii2DGrapher where
{-
import Data.Array.Unboxed
import EqManips.Types
import EqManips.Algorithm.StackVM.Stack

type ValueType = Double
type PlotRange = (ValueType, ValueType)
type DrawRange = (Int, Int)

{-plot2DExpression :: FormulaPrim -> PlotRange -> PlotRange-}
                 {--> DrawRange-}
                 {--> Either String (UArray (Int, Int) Char)-}
{-plot2DExpression formula xRange yRange (width, height) =-}
    {-either id drawer-}
      {-where drawer = eval-}
            {-eval compiledFormula x =-}
                {-evalProgram compiledFormula x 0-}

linearSucc :: ValueType -> ValueType -> ValueType
linearSucc nextVal = (nextVal +)

logSucc :: ValueType -> ValueType
logSucc = (10 *)

rangeSplitter :: (ValueType -> ValueType) -> ValueType
              -> ValueType
rangeSplitter f x = x + (f x - x) / 2

data DrawAction = ActionStop 
                | Subdivide Char
                | Continue Char

neighbour :: Int -> ValueType -> ValueType -> Bool
neighbour _ y1 y2 = False

type Scaler = ValueType -> Int

charOf :: Int -> Scaler -> ValueType -> ValueType 
       -> DrawAction
charOf height yplot y1 y2
    | yplot y1 > height || yplot y1 < 0 =
        ActionStop
    | yplot y1 > yplot y2 || yplot y1 < yplot y2 =
        Subdivide '|'
    | neighbour height y1 y2 = Continue '-'
    | y1 < y2 = Continue '/'
    | y1 > y2 = Continue '\\'

plot2D :: (Int, Int) -> ValueType -> (ValueType -> ValueType)
       -> (ValueType -> ValueType)
       -> Scaler -> Scaler -> ValueType
       -> [((Int, Int),Char)]
plot2D (width, height) xEnd f xSucc xPlot yPlot = subPlot
  where subPlot x
          | x > xEnd = []
          | otherwise = case charOf height yPlot 
                              (f x) (f $ xSucc x) of 
            ActionStop -> subPlot $ f x
            Subdivide c -> ((xPlot x, yPlot val),c) : 
                                inner ++ subPlot xNext
                where midPoint = (x + xNext) / 2
                      xNext = xSucc x
                      val = f x
                      inner = plot2D (width, height)
                                     xNext f (rangeSplitter xSucc)
                                     xPlot yPlot midPoint

            Continue c -> ((xPlot x, yPlot $ f x), c)
                            : subPlot (xSucc x)
-}
