module EqManips.Renderer.Ascii2DGrapher where
-- | This module implement an ASCII Art graph plotter,
-- using subdivision to provide good looking ascii graph.
module EqManips.Renderer.Ascii2DGrapher(
                                       -- * Plotting configuration
                                         PlotConf( .. )
                                       , ScalingType( .. )
                                       , Dimension( .. )
                                       , defaultPlotConf
                                       -- * Da Ploting LAUNCHER !!
                                       , plot2DExpression
                                       ) where

import Data.Array.Unboxed
import Text.Printf

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

data Dimension = Dimension
    { minVal :: ValueType
    , maxVal :: ValueType
    , projectionSize :: Int
    , scaling :: ScalingType
    , drawAxis :: Bool
    , labelPrecision :: Int
    , labelEvery :: Maybe Int
    }
    deriving Show

data PlotConf = PlotConf
    { xDim :: Dimension
    , yDim :: Dimension
    , draw0Axis :: Bool
    , graphTitle :: Maybe String
    }
    deriving Show

defaultPlotConf :: PlotConf
defaultPlotConf = PlotConf
    { xDim = Dimension
        { minVal = 0.0
        , maxVal = 10.0
        , projectionSize = 50
        , scaling = Linear
        , drawAxis = False
        , labelPrecision = 4
        , labelEvery = Just 7
        }

    , yDim = Dimension
        { minVal = -5.0
        , maxVal = 5.0
        , projectionSize = 30
        , scaling = Linear
        , drawAxis = False
        , labelPrecision = 4
        , labelEvery = Just 4
        }

    , draw0Axis = False
    , graphTitle = Nothing
    }

doubleShow :: Dimension -> ValueType -> String
doubleShow dim = printf "%.*f" (labelPrecision dim)

dimensionRange :: Dimension -> PlotRange
dimensionRange dim = (minVal dim, maxVal dim)

canvasSize :: PlotConf -> (Int, Int)
canvasSize conf = ( projectionSize $ xDim conf
                  , projectionSize $ yDim conf)

-- | Translate a list of write on the x (width) axis with
-- a given amount. Perform no operation if translation amount
-- is 0.
translateX :: Int -> [((Int, Int), Char)] -> [((Int, Int), Char)]
translateX 0 lst = lst
translateX i lst = [ ((x + i, y), c) | ((x,y), c) <- lst ]

-- | Same thing as 'translateX' but with the y (height) axis.
translateY :: Int -> [((Int, Int), Char)] -> [((Int, Int), Char)]
translateY 0 lst = lst
translateY i lst = [ ((x, y + i), c) | ((x,y), c) <- lst ]

-- | Add some vertical labels
addYAxisLabel :: Dimension -> ValSuccessor -> CharCanvas -> CharCanvas
addYAxisLabel dim successor rez@(((xPos, shiftHeight), adds), vals) =
 case (drawAxis dim, labelEvery dim) of
  (_, Nothing) -> rez
  (False, _) -> rez
  (True, Just size) ->
   (((xShift, shiftHeight), adds), vals' ++ draw shiftHeight (minVal dim))
    where maxHeight = projectionSize dim + shiftHeight

          xShift = max 8 xPos
          vals' = translateX (xShift - xPos) vals
          
          apply val 0 = val
          apply val times = apply (successor val) $ times - 1

          draw y yVal
            | y >= maxHeight = []
            | otherwise = 
                let indicator = ((xShift - 1, y), '+')
                    future = draw (y + size) (apply yVal size)
                in indicator :
                    [((xP, y), c) | (xP, c) <- zip [0.. xShift - 2] 
                                            $ doubleShow dim yVal] ++
                                    future

-- | Represent a tuple of canvas extension and a list
-- of characters. It's ((leftAdd, bottomAdd), (rightAdd, topAdd))
type CharCanvas =
    (((Int,Int),(Int,Int)), [((Int,Int), Char)])

addXAxisLabel :: Dimension -> ValSuccessor -> CharCanvas -> CharCanvas
addXAxisLabel dim successor rez@(((shiftWidth, yPos), (addX, addY)), vals) = 
 case (drawAxis dim, labelEvery dim) of
  (_, Nothing) -> rez
  (False, _) -> rez
  (True, Just size) ->
   (((shiftWidth, yPos)
   ,(rightShift, addY) ), vals ++ draw shiftWidth (minVal dim))
    where maxWidth = projectionSize dim + shiftWidth

          apply val 0 = val
          apply val times = apply (successor val) $ times - 1

          rightShift = max addX 
                     $ size - (projectionSize dim `rem` size)

          draw x xVal
            | x >= maxWidth = []
            | otherwise = 
                let indicator = ((x - 1,1), '|')
                    future = draw (x + size) (apply xVal size)
                in indicator : [((xPos, 0), c)
                                    | (xPos, c) <- zip [x - 1.. x + size - 3] 
                                                    $ doubleShow dim xVal] ++ future
                
addTitle :: PlotConf -> Maybe String -> CharCanvas -> CharCanvas
addTitle _ Nothing a = a
addTitle conf (Just t) (((shiftWidth, shiftHeight), adds), vals) =
    (((shiftWidth, shiftHeight + 2), adds), toAdd ++ translateY 2 vals)
        where begin = (projectionSize (xDim conf) - length t) `div` 2
              toAdd = [((x,0), c) | (x,c) <- zip [begin ..] t]

add0Axis :: PlotConf -> Scaler -> CharCanvas -> CharCanvas 
add0Axis conf scaler original@(((shiftWidth, shiftHeight), adds), vals) =
    if y < 0 then original else
    ( ((wShift, shiftHeight), adds)
    , ((wShift - nominalShift + 1, y), '0') : 
        line ++ translateX valShift vals)
    where w = projectionSize $ xDim conf
          h = projectionSize $ yDim conf
          y = scaler 0
          line = if y >= 0 && y < h
            then [((x, y), '-') | 
                    x <- [wShift .. wShift + (w - 1)]]
            else []
          nominalShift = 4
          wShift = max nominalShift shiftWidth
          valShift = if shiftWidth >= nominalShift
            then shiftWidth - wShift
            else wShift - shiftWidth

addYAxis :: PlotConf -> Scaler -> CharCanvas -> CharCanvas
addYAxis conf _scaler (((shiftWidth, shiftHeight), adds), vals) =
    ( ((wShift, shiftHeight), adds)
    ,  line ++ translateX valShift vals)
    where h = projectionSize $ yDim conf
          x = nominalShift - 1
          line = [((x, y), '|') | 
                    y <- [shiftHeight .. shiftHeight + (h - 1)]]
          nominalShift = 4
          wShift = max nominalShift shiftWidth
          valShift = if shiftWidth >= nominalShift
            then shiftWidth - wShift
            else wShift - shiftWidth


addXaxis :: PlotConf -> Scaler -> CharCanvas -> CharCanvas
addXaxis conf _ (((shiftWidth, shiftHeight), adds), vals) =
  ( ((shiftWidth, hShift), adds)
  , line ++ translateY valShift vals)
    where line = [((x, hShift - 1), '_') 
                        | x <- [shiftWidth ..(w - 1) + shiftWidth]]
          w = projectionSize $ xDim conf
          nominalShift = 2
          hShift = max nominalShift shiftHeight
          valShift = hShift - shiftHeight

-- | Equivalent of 'when' but non-monadic.
doWhen :: Bool -> (a -> a) -> a -> a
doWhen False _ a = a
doWhen True  f a = f a

-- | Function in charge of adding all the plot axis
-- to the generated character stream
addAxis :: PlotConf
        -> (Scaler, Scaler)
        -> (ValSuccessor, ValSuccessor)
        -> [((Int, Int), Char)]
        -> CharCanvas
addAxis conf (widthScaler, heightScaler) (xSucc, ySucc) a = 
      doWhen (graphTitle conf /= Nothing)
             (addTitle conf $ graphTitle conf)
    . doWhen (labelEvery (yDim conf) /= Nothing)
             (addYAxisLabel (yDim conf) ySucc)
    . doWhen (drawAxis $ yDim conf)
             (addYAxis conf heightScaler)
    . doWhen (labelEvery (xDim conf) /= Nothing)
             (addXAxisLabel (xDim conf) xSucc)
    . doWhen (drawAxis $ xDim conf)
             (addXaxis conf widthScaler)
    . doWhen (draw0Axis conf)
             (add0Axis conf heightScaler) $ (((0,0), (0,0)), a)


-- | User function to start a plot. Handle all the scary
-- configuration before starting the plot.
plot2DExpression :: PlotConf -> FormulaPrim
                 -> Either String (UArray (Int, Int) Char)
plot2DExpression conf formula =
    case VM.compileExpression formula of
      Left err -> Left err
      Right prog ->
        let successor = widthSuccessor $ xDim conf
            (_,ySuccessor) = widthSuccessor $ yDim conf
            yScaler = sizeMapper $ yDim conf
            xScaler = sizeMapper $ xDim conf
            (xBegin, xEnd) = dimensionRange $ xDim conf
            size@(w, h)  = canvasSize conf
            graph = plot2D size xEnd
                           (flip (VM.evalProgram prog) 0)
                           successor xScaler yScaler
                           xBegin
            (((shiftX, shiftY), (addX, addY)), graph') =
                addAxis conf (xScaler, yScaler) (snd successor, ySuccessor) graph
        in Right $ accumArray (\_ e -> e) ' '
                              ((0, 0) ,(w + shiftX + addX - 1, h + shiftY + addY - 1)) $
                              [v | v@((x,_),_) <- graph', 
                                                 x < w + shiftX + addX,
                                                 x >= 0]


-- | This type is a transformation from function
-- result to screen space.
type Scaler = ValueType -> Int

-- | Function used to find the next \'x\' element
-- to be plotted.
type ValSuccessor =
    ValueType -> ValueType

-- | Equivalent of the 'succ' function of the
-- 'Enum' class, with a linear scale.
widthSuccessor :: Dimension -> (ValSuccessor, ValSuccessor)
widthSuccessor dim = case (scaling dim, minVal dim > 0) of
  (Linear, _) -> (\v -> v - addVal, \v -> v + addVal)
    where addVal = (vMax - vMin) / toEnum (projectionSize dim - 2)
          (vMin, vMax) = dimensionRange dim
  (Logarithmic, True)  -> (\v -> v / mulVal,\v -> v * mulVal)
    where mulVal = (vMax / vMin) ** (1.0 / toEnum (projectionSize dim - 1))
          (vMin, vMax) = dimensionRange dim
  (Logarithmic, False) -> (\v -> vPrev (v + vAdd) - vAdd
                          ,\v -> vNext (v + vAdd) - vAdd)
    where (vMin, vMax) = dimensionRange dim
          bigpsilon = 0.1
          vAdd = 0.1 + negate vMin
          (vPrev, vNext) = widthSuccessor $ 
                dim { minVal = bigpsilon
                    , maxVal = vMax - vMin + bigpsilon}
          


-- | How to map the height value onto the screen,
-- by taking tinto action the 'canvas' size
sizeMapper :: Dimension -> (ValueType -> Int)
sizeMapper dim = 
 let (vMin, vMax) = dimensionRange dim
     fullSize = projectionSize dim
 in case (scaling dim, vMin > 0) of
   (Linear, _) -> \val -> truncate $ (val - vMin) * scaler
      where scaler = toEnum fullSize / (vMax - vMin + 1)

   (Logarithmic, True) -> \val -> truncate $ (log val - vMin') * scaler
      where (vMin', vMax') = (log vMin, log vMax)
            scaler = toEnum fullSize / (abs (vMax' - vMin') + 1)

   (Logarithmic, False) -> \val -> truncate $ (log $ val - vMin') * scaler
      where (vMin', vMax') = (log 0.1, log $ vMax - vMin)
            scaler = toEnum fullSize / (abs (vMax' - vMin') + 1)

              
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

