-- | This module implement an ASCII Art graph plotter,
-- using subdivision to provide good looking ascii graph.
module EqManips.Renderer.Ascii2DGrapher(
                                       -- * Plotting configuration
                                         PlotConf( .. )
                                       , ScalingType( .. )
                                       , defaultPlotConf
                                       -- * Da Ploting LAUNCHER !!
                                       , plot2DExpression
                                       ) where

import Data.Array.Unboxed
import EqManips.Types
import qualified EqManips.Algorithm.StackVM.Stack as VM

-- | Alias in case I want to change in the future.
type ValueType = Double

-- | (Begin, End), all inclusive
type PlotRange = (ValueType, ValueType)

data ScalingType =
      Linear
    | Logarithmic
    deriving Show

data PlotConf = PlotConf
    { xRange :: PlotRange
    , yRange :: PlotRange
    , drawWidth :: Int
    , drawHeight :: Int
    , yScaling :: ScalingType
    , xScaling :: ScalingType
    , drawXAxis :: Bool
    , drawYAxis :: Bool
    , draw0Axis :: Bool
    }
    deriving Show

defaultPlotConf :: PlotConf
defaultPlotConf = PlotConf
    { xRange = (0.0, 10.0)
    , yRange = (-5.0, 5.0)
    , drawWidth = 50
    , drawHeight = 30
    , xScaling = Linear
    , yScaling = Linear
    , drawXAxis = False
    , drawYAxis = False
    , draw0Axis = False
    }

translateX :: Int -> [((Int, Int), Char)] -> [((Int, Int), Char)]
translateX i lst = [ ((x + i, y), c) | ((x,y), c) <- lst ]

translateY :: Int -> [((Int, Int), Char)] -> [((Int, Int), Char)]
translateY i lst = [ ((x, y + i), c) | ((x,y), c) <- lst ]

add0Axis :: PlotConf -> Scaler -> (Int, Int)
         -> [((Int, Int), Char)]
         -> ((Int, Int), [((Int, Int), Char)])
add0Axis conf scaler (shiftWidth, shiftHeight) vals =
    ( (wShift, shiftHeight)
    , ((wShift - 1,y), '0') : line ++ translateX valShift vals)
    where w = drawWidth conf
          y = scaler 0
          line = [((x, y), '-') | 
                    x <- [wShift .. wShift + (w - 1)]]
          wShift = max 3 shiftWidth
          valShift = shiftWidth - wShift

addXaxis :: PlotConf -> Scaler -> (Int, Int)
         -> [((Int, Int), Char)]
         -> ((Int, Int), [((Int, Int), Char)])
addXaxis conf _ (shiftWidth, shiftHeight) vals =
  ( (shiftWidth, hShift)
  , line ++ translateY valShift vals)
    where line = [((x, hShift - 1), '_') 
                        | x <- [hShift..(w - 1) + hShift]]
          w = drawWidth conf
          hShift = max 2 shiftHeight
          valShift = shiftHeight - hShift

addAxis :: PlotConf
        -> (Scaler, Scaler)
        -> [((Int, Int), Char)]
        -> ((Int, Int), [((Int, Int), Char)])
addAxis conf (widthScaler, heightScaler) a = if drawYAxis conf
                then (shifts'', vals'')
                else (shifts'', vals'')
    where shifts = (0,0)
          (shifts', vals') = if draw0Axis conf
                then add0Axis conf heightScaler shifts a
                else (shifts, a)
          (shifts'', vals'') = if drawXAxis conf
                then addXaxis conf widthScaler shifts' vals'
                else (shifts', vals')

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
            ((shiftX, shiftY), graph') =
                addAxis conf (xScaler, yScaler) graph
        in Right $ accumArray (\_ e -> e) ' '
                              ((0, 0) ,(w + shiftX - 1, h + shiftY - 1))
                              [v | v@((x,_),_) <- graph', x < w + shiftX]
    

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
widthSuccessor conf Linear = (\x -> x - addVal, \x -> x + addVal)
    where addVal = (xMax - xMin) / toEnum (drawWidth conf)
          (xMin, xMax) = xRange conf
{-widthSuccessor _ (Logarithmic v) x = v * x-}

-- | How to map the height value onto the screen,
-- by taking tinto action the 'canvas' size
sizeMapper :: PlotRange -> Int -> ScalingType
           -> (ValueType -> Int)
sizeMapper (vMin, vMax) fullSize Linear =
  \val -> truncate $ (val - vMin) * scaler
      where scaler = toEnum fullSize / (vMax - vMin + 1)
              
sizeMapper (vMin, vMax) fullSize Logarithmic =
  \val -> truncate $ (log val - vMin') * scaler
      where (vMin', vMax') = (log vMin, log vMax)
            scaler = toEnum fullSize / (vMax' - vMin')

-- | Describe the action that the plotter must
-- accomplish in order to draw a function
data DrawAction =
    ActionStop          -- ^ Stop the ploting/subdivision for this value
  | SubdivideBoth  Char -- ^ Halve the x interval and continue plotting, on both ends
  | SubdivideUpper Char -- ^ Halve and continue only on the upper part.
  | SubdivideLower Char -- ^ Halve and continue only on the lower part.
  | SubdivideIgnore     -- ^ Halve and continue both ends but don't write any char.
  | Continue Char       -- ^ Continue with the current interval, adn write a char.

neighbour :: ValueType -> ValueType -> Bool
neighbour y1 y2 = abs (y1 - y2) < 0.05

-- | Given a successor function given as parameter,
-- it will return a successor function going half
-- as far as the previous one. Work with backward
-- functions to.
rangeSplitter :: ValSuccessor -> ValSuccessor
rangeSplitter f x = x + (f x - x) / 2

-- | As side is inversed when drawing backward,
-- this function help to choose a representation
-- given the current direction and a 'Forward'
-- assention or 'Backward' descent.
sideChar :: Direction           -- ^ Current drawing direction
         -> Direction          -- ^ Assention or descent
         -> Char
sideChar Forward Forward = '/'
sideChar Forward Backward = '\\'
sideChar Backward a = sideChar Forward $ inverseDirection a

-- | Given two samples, give an Ascii representation
-- and information to the plotter on how to continue
-- the drawing.
charOf :: Direction        -- ^ Current plotting direction
       -> Int              -- ^ Canvas height
       -> Int              -- ^ Absciss in canvas space of the previous value.
       -> (ValueType, Int) -- ^ Value and canvas position of the current value.
       -> (ValueType, Int) -- ^ Value and canvas position of the current value.
       -> DrawAction       -- ^ What to do next
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


-- | Happy float
epsilon :: ValueType
epsilon = 0.00000000000001

-- | Type used when plotting, to inform
-- the subdivision direction.
data Direction = Forward | Backward
    deriving Eq

-- | Inverse the direction, equivalent of
-- 'not', but for 'Direction'
inverseDirection :: Direction -> Direction
inverseDirection Forward = Backward
inverseDirection Backward = Forward

-- | The real plotting function, calling it is rather complex,
-- due to the number of thing to take into account, favor the use
-- of a more high level function like 'plot2DExpression'
plot2D :: (Int, Int)              -- ^ Size of the canvas in number of cells
       -> ValueType               -- ^ End value for x
       -> (ValueType -> ValueType) -- ^ The function to be evaluated
       -> (ValSuccessor, ValSuccessor)  -- ^ x Successor function, backward, forward,
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

