module EqManips.Renderer.Ascii2DGrapher{-  (
                                       -- * Plotting configuration
                                         PlotConf( .. )
                                       , ScalingType( .. )
                                       , defaultPlotConf
                                       -- * Da Ploting LAUNCHER !!
                                       , plot2DExpression
                                       ) -} where

import Data.Array.Unboxed
import EqManips.Types
import Debug.Trace
import qualified EqManips.Algorithm.StackVM.Stack as VM

type ValueType = Double
type PlotRange = (ValueType, ValueType)

data ScalingType =
      Linear        ValueType
    | Logarithmic   ValueType
    deriving Show

data PlotConf = PlotConf
    { xRange :: PlotRange
    , yRange :: PlotRange
    , drawWidth :: Int
    , drawHeight :: Int
    , yScaling :: ScalingType
    , xScaling :: ScalingType
    , drawAxis :: Bool
    }
    deriving Show

defaultPlotConf :: PlotConf
defaultPlotConf = PlotConf
    { xRange = (0.0, 10.0)
    , yRange = (-5.0, 5.0)
    , drawWidth = 50
    , drawHeight = 30
    , xScaling = Linear 1.0
    , yScaling = Linear 1.0
    , drawAxis = False
    }

plot2DExpression :: PlotConf -> FormulaPrim
                 -> Either String (UArray (Int, Int) Char)
plot2DExpression conf formula =
    case VM.compileExpression formula of
      Left err -> Left err
      Right prog ->
        let successor = widthSuccessor conf $ xScaling conf
            yScaler = sizeMapper (yRange conf) (drawHeight conf)
                    $ yScaling conf
            xScaler = sizeMapper (xRange conf) (drawWidth conf)
                    $ xScaling conf
            (xBegin, xEnd) = xRange conf
            size@(w, h)  = (drawWidth conf, drawHeight conf)
            graph = plot2D size xEnd
                           (flip (VM.evalProgram prog) 0)
                           successor xScaler yScaler
                           xBegin
        in Right $ accumArray (\_ e -> e) ' '
                              ((0, 0) ,(w - 1, h - 1)) $
                              {-(\a -> trace (show a) a) $-}
                              [v | v@((x,_),_) <- graph, x < w ]
    

-- | This type is a transformation from function
-- result to screen space.
type Scaler = ValueType -> Int

-- | Function used to find the next \'x\' element
-- to be plotted.
type ValSuccessor =
    ValueType -> ValueType

-- | Equivalent of the 'succ' function of the
-- 'Enum' class, with a linear scale.
widthSuccessor :: PlotConf -> ScalingType -> (ValSuccessor, ValSuccessor)
widthSuccessor conf (Linear _) = (\x -> x - addVal, \x -> x + addVal)
    where addVal = (xMax - xMin) / toEnum (drawWidth conf)
          (xMin, xMax) = xRange conf
{-widthSuccessor _ (Logarithmic v) x = v * x-}

-- | How to map the height value onto the screen,
-- by taking tinto action the 'canvas' size
sizeMapper :: PlotRange -> Int -> ScalingType
           -> (ValueType -> Int)
sizeMapper (vMin, vMax) fullSize (Linear _) =
  \val -> truncate $ (val - vMin) * scaler
      where scaler = toEnum fullSize / (vMax - vMin + 1)
              
sizeMapper (vMin, vMax) fullSize (Logarithmic _) =
  \val -> truncate $ (val - vMin') * scaler
      where (vMin', vMax') = (log vMin, log vMax)
            scaler = toEnum fullSize / (vMax' - vMin')

-- | Describe the action that the plotter must
-- accomplish in order to draw a function
data DrawAction =
    ActionStop -- ^ Stop the ploting
  | SubdivideBoth Char -- ^ Halve the x interval and continue plotting
  | SubdivideUpper Char
  | SubdivideLower Char
  | SubdivideIgnore
  | Continue Char  -- ^ Continue with the current interval
  deriving Show

neighbour :: ValueType -> ValueType -> Bool
neighbour y1 y2 = abs (y1 - y2) < 0.05

rangeSplitter :: ValSuccessor -> ValueType -> ValueType
rangeSplitter f x = x + (f x - x) / 2

sideChar :: Direction -> Direction -> Char
sideChar Forward Forward = '/'
sideChar Forward Backward = '\\'
sideChar Backward a = sideChar Forward $ inverseDirection a

-- | Given two samples, give an Ascii representation
-- and information to the plotter on how to continue
-- the drawing.
charOf :: Direction -> Int -> Int
       -> (ValueType, Int)
       -> (ValueType, Int)
       -> DrawAction
charOf direction height screenPrev (y1, screenY1) (y2, screenY2)
   | isNaN y1 = ActionStop
   | isInfinite y1 && screenY1 >= 0 && screenY1 < height =
       SubdivideBoth '|'
   | isInfinite y1 = SubdivideIgnore
   -- We are out of the drawing box, stop
   -- the drawing for the current value of x
   | screenY1 >= height || screenY1 < 0 = ActionStop
  
  
   -- The two values are in a different cell,
   -- we need to refine the values.
   | abs (screenY1 - screenY2) > 1 && abs (screenY1 - screenPrev) > 1
       = SubdivideBoth '|'
  
   | abs (screenY1 - screenY2) > 1 = SubdivideUpper '|'
  
   | abs (screenY1 - screenPrev) > 1 = SubdivideLower '|'
  
   -- If values are sufisently near, draw a flat
   -- line and continue
   | neighbour y1 y2 = Continue '-'
  
   -- We are ascending, but not enough to subdivide,
   -- continue to the next x
   | y1 < y2 = Continue $ sideChar direction Forward
  
   -- Descending...
   | y1 > y2 =  Continue $ sideChar direction Backward
  
   -- y1 more or less equal y2
   | otherwise = Continue '-'



epsilon :: ValueType
epsilon = 0.00000000000001

data Direction = Forward | Backward deriving Eq

inverseDirection :: Direction -> Direction
inverseDirection Forward = Backward
inverseDirection Backward = Forward

-- | The real plotting function, calling it is rather complex,
-- due to the number of thing to take into account, favor the use
-- of a more high level function like 'plot2DExpression'
plot2D :: (Int, Int)              -- ^ Size of the canvas in number of cells
       -> ValueType               -- ^ End value for x
       -> (ValueType -> ValueType) -- ^ The function to be evaluated
       -> (ValSuccessor, ValSuccessor)  -- ^ x Successor function, forward, backward
       -> Scaler                  -- ^ Function to translate xVal to canvas position
       -> Scaler                  -- ^ Function to translate (f xVal) to canvas position
       -> ValueType    -- ^ The \'current\' ploted value, xBegin for first call
       -> [((Int, Int),Char)] -- ^ Woohoo, the result, to be stored in an array
plot2D (_width, height) xStop f widthSucc xPlot yPlot xInit = 
 subPlot widthSucc (xInit - epsilon, xStop) Forward 0 xInit
  where subPlot successors@(xPrev, xSucc)
                interval@(xBegin, xEnd) 
                direction prevScreen x
          | direction == Forward && (x <= xBegin || x >= xEnd) = []
          | direction == Backward && (x <= xEnd || x >= xBegin) = []
          | otherwise = 
          let val = f x
              xNext = if direction == Forward then xSucc x
                                             else xPrev x
              screenY = yPlot val
              midPoint = (x + xNext) / 2
              halfSuccessors@(halfPrev, halfSucc) =
                  (rangeSplitter $ rangeSplitter xPrev
                  ,rangeSplitter $ rangeSplitter xSucc)

              (subPrev, subSucc) = if direction == Forward
                    then (halfPrev, halfSucc)
                    else (halfSucc, halfPrev)
              midInfo = yPlot $ f midPoint

              lowerRange = subPlot halfSuccessors 
                                   (midPoint, xBegin)
                                   (inverseDirection direction)
                                   midInfo 
                                   $ subPrev midPoint

              upperRange = subPlot halfSuccessors
                                   (midPoint, xNext) 
                                   direction
                                   midInfo
                                   $ subSucc  midPoint

              midChar = if midInfo > 0 && midInfo < height
                    then [((xPlot midPoint, midInfo), '|')]
                    else []
              future = subPlot successors interval direction
                               screenY xNext


          in case charOf direction height prevScreen
                         (val, screenY) (f xNext, yPlot $ f xNext) of 
            ActionStop -> future
            Continue c -> ((xPlot x, screenY), c) : future

            SubdivideLower c ->
                lowerRange ++ midChar ++ ((xPlot x, screenY),c) : future
            SubdivideUpper c ->
                upperRange ++ midChar ++ ((xPlot x, screenY),c) : future
            SubdivideBoth c ->
                lowerRange ++ upperRange ++
                    midChar ++ ((xPlot x, screenY),c) : future
            SubdivideIgnore ->
                lowerRange ++ upperRange ++ midChar ++ future

