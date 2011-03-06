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
    }

plot2DExpression :: PlotConf -> FormulaPrim
                 -> Either String (UArray (Int, Int) Char)
plot2DExpression conf formula =
    case VM.compileExpression formula of
      Left err -> Left err
      Right prog -> trace (show prog) $
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
                              ((0, 0) ,(w - 1, h - 1)) 
                                $ (\a -> trace (show a) a) graph
    

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
widthSuccessor conf (Linear _) = (\x -> tracer x $ x + addVal
                                 ,\x -> tracer x $ x - addVal)
    where addVal = (xMax - xMin) / toEnum (drawWidth conf)
          (xMin, xMax) = xRange conf
          tracer x vv = trace (">" ++ show x ++ " + " ++ show addVal
                            ++ "-->" ++ show vv) vv
{-widthSuccessor _ (Logarithmic v) x = v * x-}

-- | How to map the height value onto the screen,
-- by taking tinto action the 'canvas' size
sizeMapper :: PlotRange -> Int -> ScalingType
           -> (ValueType -> Int)
sizeMapper (vMin, vMax) fullSize (Linear _) =
  \val -> tracer val $ truncate $ (val - vMin) * scaler
      where scaler = toEnum fullSize / (vMax - vMin)
            tracer val x = trace ("v(" ++ show vMin ++ ":" 
                                   ++ show vMax ++ ")@"
                                   ++ show fullSize
                                   ++ "*" ++ show scaler
                                   ++ "  "
                                   ++ show val
                                   ++ "->"
                                   ++ show x) x
              
sizeMapper (vMin, vMax) fullSize (Logarithmic _) =
  \val -> truncate $ (val - vMin') * scaler
      where (vMin', vMax') = (log vMin, log vMax)
            scaler = toEnum fullSize / (vMax' - vMin')

-- | Describe the action that the plotter must
-- accomplish in order to draw a function
data DrawAction =
    ActionStop -- ^ Stop the ploting
  | Subdivide Char -- ^ Halve the x interval and continue plotting
  | Continue Char  -- ^ Continue with the current interval
  deriving Show

neighbour :: Int -> ValueType -> ValueType -> Bool
neighbour _ y1 y2 = abs (y1 - y2) < 0.05

rangeSplitter :: ValSuccessor -> ValueType -> ValueType
rangeSplitter f x = tracer $ x + (f x - x) / 2
    where tracer vv = trace ("|>" ++ show x ++ "-->" ++ show vv) vv

-- | Given two samples, give an Ascii representation
-- and information to the plotter on how to continue
-- the drawing.
charOf :: Int -> Scaler -> Int -> ValueType -> ValueType 
       -> DrawAction
charOf height yplot screenY1 y1 y2 = check
    where screenY2 = yplot y2
          check -- Assured to win
                | isInfinite y1 = Subdivide '|'

                | isNaN y1 = ActionStop

                -- We are out of the drawing box, stop
                -- the drawing for the current value of x
                | screenY1 >= height || screenY1 < 0 = ActionStop


                -- The two values are in a different cell,
                -- we need to refine the values.
                | screenY1 < screenY2 - 1 = Subdivide '|'
                | screenY1 > screenY2 + 1= Subdivide '|'

                -- If values are sufisently near, draw a flat
                -- line and continue
                | neighbour height y1 y2 = Continue '-'
            
                -- We are ascending, but not enough to subdivide,
                -- continue to the next x
                | y1 < y2 = Continue '/'
            
                -- Descending...
                | y1 > y2 = Continue '\\'

                -- y1 more or less equal y2
                | otherwise = Continue '-'


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
plot2D (_width, height) xStop f widthSucc xPlot yPlot v = 
 subPlot (v - 0.000000000001, xStop) widthSucc True v
  where subPlot interval@(xBegin, xEnd) successors@(xSucc, xPrev) forward x
          | x <= xBegin || x >= xEnd = []
          | otherwise = 
          let val = f x
              xNext = if forward then xSucc x else xPrev x
              screenY = yPlot val
              tracer a = trace (show a) a
          in case tracer $ charOf height yPlot screenY val (f xNext) of 
            ActionStop -> subPlot interval successors forward xNext
            Continue c -> ((xPlot x, screenY), c)
                            : subPlot interval successors forward xNext

            Subdivide c -> ((xPlot x, screenY),c) : 
                            innerForward ++ innerBackward 
                            ++ subPlot interval successors forward xNext
                where midPoint = (x + xNext) / 2
                      halfSuccessors = (rangeSplitter xSucc,
                                        rangeSplitter xPrev)
                      innerForward = subPlot (x, min xEnd xNext)
                                             halfSuccessors     
                                             True midPoint
                      innerBackward = subPlot (x, midPoint) halfSuccessors False
                                              midPoint

