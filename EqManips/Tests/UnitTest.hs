module EqManips.Tests.UnitTest( eqUnittests, runEqTests ) where

import Control.Applicative
import System.Directory  
import System.FilePath
import Data.Ratio

import Test.HUnit
import Test.HUnit.Text
import EqManips.Types
import EqManips.EvaluationContext
import EqManips.Renderer.Ascii
import EqManips.Renderer.Sexpr
import EqManips.Renderer.Latex
import EqManips.Renderer.Mathml
import EqManips.Renderer.RenderConf
import EqManips.Algorithm.Utils
import EqManips.Algorithm.Eval
import EqManips.InputParser.EqCode hiding( expr )

infixr 1 ==>

int :: Integer -> FormulaPrim
int = CInteger

float :: Double -> FormulaPrim
float = CFloat

frac :: (Integer, Integer) -> FormulaPrim
frac (a,b) = Fraction $ a % b

var :: String -> FormulaPrim
var = Variable

runEqTests :: IO Bool
runEqTests = do
    iotest <- formatTestList 
    rez <- runTestTT . TestList $ eqUnittests : iotest
    return $ errors rez == 0 && failures rez == 0

eqUnittests :: Test
eqUnittests = TestList $
    [ TestLabel (sexprRender (Formula $ binOp OpEq [toEval, rez]) ++ " ")
    . TestCase
    $ evalResultCheck toEval rez | (toEval, rez) <- arithmeticTest
                                                 ++ polynomeTest
                                                 ++ basicManualFunction
                                                 ++ comparisonOperator 
                                                 ++ indexationOperation
                                                 ++ listCreationOperation ]

    ++ [ TestLabel (sexprRender (Formula $ binOp OpEq [toEval, rez]) ++ " ")
       . TestCase
       $ exactevalResultCheck toEval rez | (toEval, rez) <- exactArithmeticTest ]

    ++ [ TestLabel (show expr ++ " = " ++ show rez)
       . TestCase
       $ assertEqual "" rez expr | (expr, rez) <- coeffArithmeticTest ]

    ++ [ TestLabel (show toEval ++ " = " ++ show rez)
       . TestCase
       $ evalResultCheck toEval rez | (x, x') <- [ (CInteger 1, 1.0)
                                                 , (CInteger 2, 2.0)
                                                 , (CFloat 0.5, 0.5) ]
                                    , (toEval, rez) <- basicFunctions x x']

(==>) :: a -> b -> (a,b)
(==>) = (,)

exactArithmeticTest :: [(FormulaPrim, FormulaPrim)]
exactArithmeticTest =
    [ -- Fraction -> Fraction -> Fraction
      frac (1,2) + frac (1,2) ==> int 1
    , frac (1,2) + frac (1,4) ==> frac (3,4)

    , frac (1,2) - frac (1,2) ==> int 0
    , frac (1,2) - frac (1,4) ==> frac (1,4)

    , frac (1,3)   * frac (1, 4) ==> frac (1,12)
    , frac (-1, 4) * frac (2, 4) ==> frac (-1, 8)

    , frac (1,3)   / frac (1, 4) ==> frac (4,3)
    , frac (-1, 4) / frac (2, 4) ==> frac (-1, 2)

    , complex (int 0, int 4) / complex (int 1, int 2) ==> complex (frac (8,5), frac (4,5))
    ]

arithmeticTest :: [(FormulaPrim, FormulaPrim)]
arithmeticTest =
    [ -- Int -> Int -> Int
      int 2 + int 3 ==> int 5
    , int 2 * int 3 ==> int 6
    , int 2 - int 3 ==> negate (int 1)
    , int 2 / int 3 ==> float (2.0 / 3.0)

    , int 4 / int 2 ==> int 2
    , int 2 / int 2 ==> int 1

    -- Float -> Float -> Float
    , float 2 + float 3 ==> float 5
    , float 2 * float 3 ==> float 6
    , float 2 - float 3 ==> float (-1)
    , float 2 / float 3 ==> float (2.0 / 3.0)

    , float 4 / float 2 ==> float 2
    , float 2 / float 2 ==> float 1


    -- Int -> (-Int) -> +-Int
    , int 2 + negate (int 1) ==> int 1
    , int 5 - negate (int 7) ==> int 12
    , int 2 * negate (int 3) ==> negate (int 6)
    , int 4 / negate (int 2) ==> negate (int 2)

    -- (-Int) -> Int -> +-Int
    , negate (int 2) + int 1 ==> negate (int 1)
    , negate (int 5) - int 7 ==> negate (int 12)
    , negate (int 2) * int 3 ==> negate (int 6)
    , negate (int 4) / int 2 ==> negate (int 2)

    -- (-Int) -> (-Int) -> +-Int
    , negate (int 2) + negate (int 1) ==> negate (int 3)
    , negate (int 5) - negate (int 7) ==> int 2
    , negate (int 2) * negate (int 3) ==> int 6
    , negate (int 4) / negate (int 2) ==> int 2

    -- Float -> Int -> Float
    , float 2 + int 3 ==> float 5
    , float 2 * int 3 ==> float 6
    , float 2 - int 3 ==> float (-1)
    , float 2 / int 3 ==> float (2.0 / 3.0)

    -- Int -> Float -> Float
    , int 2 + float 3 ==> float 5
    , int 2 * float 3 ==> float 6
    , int 2 - float 3 ==> float (-1)
    , int 2 / float 3 ==> float (2.0 / 3.0)

    -- Float -> (-Int) -> Float
    , float 2 + negate (int 3) ==> float (-1)
    , float 2 * negate (int 3) ==> float (-6)
    , float 2 - negate (int 3) ==> float 5
    , float 2 / negate (int 3) ==> float (-2.0 / 3.0)

    -- (-Int) -> Float -> Float
    , negate (int 2) + float 3 ==> float 1
    , negate (int 2) * float 3 ==> float (-6)
    , negate (int 2) - float 3 ==> float (-5)
    , negate (int 2) / float 3 ==> float (-2.0 / 3.0)

    -- Complex -> Int -> Complex
    , complex (int 0, int 2) + int 1 ==> complex (int 1, int 2)
    , complex (int 1, int 2) - int 1 ==> complex (int 0, int 2)
    , complex (int 4, int 2) * int 2 ==> complex (int 8, int 4)
    , complex (int 4, int 2) / int 2 ==> complex (int 2, int 1)

    -- Complex -> Float -> Complex
    , complex (int 0, int 2) + float 1 ==> complex (float 1, int 2)
    , complex (int 1, int 2) - float 1 ==> complex (int   0, int 2)
    , complex (int 4, int 2) * float 2 ==> complex (float 8, float 4)
    , complex (int 4, int 2) / float 2 ==> complex (float 2, float 1)

    -- Complex -> Complex -> Complex
    , complex (int 0, int 1) + complex (int 1, int 2) ==> complex (int 1, int 3)
    , complex (int 2, int 3) - complex (int 1, int 2) ==> complex (int 1, int 1)
    , complex (int 0, int 1) * complex (int 1, int 2) ==> complex (negate $ int 2, int 1)

    ]

polynomeTest :: [(FormulaPrim, FormulaPrim)]
polynomeTest = [
    -- Polynome/Int
      poly (Polynome "x" [(0, PolyRest 1),(1, PolyRest 2),(3, PolyRest 4)]) * int 2 ==>
        poly (Polynome "x" [(0, PolyRest 2),(1, PolyRest 4),(3, PolyRest 8)])

    , int 2 * poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 4)]) ==>
        poly (Polynome "x" [(0, PolyRest 2) ,(1, PolyRest 4) ,(3, PolyRest 8)])

    , poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 4)]) + int 2 ==>
        poly (Polynome "x" [(0, PolyRest 3) ,(1, PolyRest 2) ,(3, PolyRest 4)])

    , int 2 + poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 4)]) ==>
        poly (Polynome "x" [(0, PolyRest 3) ,(1, PolyRest 2) ,(3, PolyRest 4)])

    , poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 4)]) - int 2 ==>
        poly (Polynome "x" [(0, PolyRest (-1)) ,(1, PolyRest 2) ,(3, PolyRest 4)])

    , int 2 - poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 4)]) ==>
        poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest (-2)) ,(3, PolyRest (-4))])

    -- Polynome/Int without x^0
    , poly (Polynome "x" [(1, PolyRest 2),(3, PolyRest 4)]) * int 2 ==>
        poly (Polynome "x" [(1, PolyRest 4),(3, PolyRest 8)])

    , int 2 * poly (Polynome "x" [(1, PolyRest 2) ,(3, PolyRest 4)]) ==>
        poly (Polynome "x" [(1, PolyRest 4) ,(3, PolyRest 8)])

    , poly (Polynome "x" [(1, PolyRest 2) ,(3, PolyRest 4)]) + int 2 ==>
        poly (Polynome "x" [(0, PolyRest 2) ,(1, PolyRest 2) ,(3, PolyRest 4)])

    , int 2 + poly (Polynome "x" [(1, PolyRest 2) ,(3, PolyRest 4)]) ==>
        poly (Polynome "x" [(0, PolyRest 2) ,(1, PolyRest 2) ,(3, PolyRest 4)])

    , poly (Polynome "x" [(1, PolyRest 2) ,(3, PolyRest 4)]) - int 2 ==>
        poly (Polynome "x" [(0, PolyRest (-2)) ,(1, PolyRest 2) ,(3, PolyRest 4)])

    , int 2 - poly (Polynome "x" [(1, PolyRest 2) ,(3, PolyRest 4)]) ==>
        poly (Polynome "x" [(0, PolyRest 2) ,(1, PolyRest (-2)) ,(3, PolyRest (-4))])

    --------------------------------------------------
    ----            Polynome/Polynome
    --------------------------------------------------
    -- Polynome/Polynome (same variable)
    , poly (Polynome "x" [(1, PolyRest 2) ,(3, PolyRest 4)]) +
      poly (Polynome "x" [(1, PolyRest 2) ,(3, PolyRest 4)])
      ==> poly (Polynome "x" [(1, PolyRest 4) ,(3, PolyRest 8)])

    , poly (Polynome "x" [(0, PolyRest 2) ,(12, PolyRest 4)]) +
      poly (Polynome "x" [(1, PolyRest 2) ,(3, PolyRest 4)])
      ==> poly (Polynome "x" [(0,PolyRest 2), (1, PolyRest 2) ,(3, PolyRest 4), (12, PolyRest 4)])

    
    , poly (Polynome "x" [(1, PolyRest 2) ,(12, PolyRest 4)]) +
      poly (Polynome "x" [(1, PolyRest 2) ,(3, PolyRest 4)])
      ==> poly (Polynome "x" [(1, PolyRest 4) ,(3, PolyRest 4), (12, PolyRest 4)])

    , poly (Polynome "x" [(1, PolyRest 2) ,(3, PolyRest 5)]) -
      poly (Polynome "x" [(1, PolyRest 2) ,(3, PolyRest 4)])
      ==> poly (Polynome "x" [(3, PolyRest 1)])

    , poly (Polynome "x" [(0, PolyRest 2) ,(12, PolyRest 4)]) -
      poly (Polynome "x" [(1, PolyRest 2) ,(3, PolyRest 4)])
      ==> poly (Polynome "x" [(0,PolyRest 2), (1, PolyRest (-2)) ,(3, PolyRest (-4)), (12, PolyRest 4)])

    , poly (Polynome "x" [(1, PolyRest 2) ,(12, PolyRest 4)]) -
      poly (Polynome "x" [(1, PolyRest 2) ,(3, PolyRest 4)])
      ==> poly (Polynome "x" [(3, PolyRest (-4)), (12, PolyRest 4)])

    -- Polynome/Polynome (different variable)
    , poly (Polynome "x" [(1, PolyRest 2) ,(3, PolyRest 4)]) +
      poly (Polynome "y" [(1, PolyRest 2) ,(3, PolyRest 4)])
      ==> poly (Polynome "x" [ (0,Polynome "y" [(1, PolyRest 2) ,(3, PolyRest 4)])
                             , (1, PolyRest 2)
                             , (3, PolyRest 4)])

    , poly (Polynome "x" [(0, PolyRest 2), (1, PolyRest 2), (3, PolyRest 4), (12, PolyRest 4)]) +
      poly (Polynome "y" [(1, PolyRest 2) ,(3, PolyRest 4)])
      ==> poly (Polynome "x" [ (0, Polynome "y" [(0, PolyRest 2), (1, PolyRest 2) ,(3, PolyRest 4)])
                             , (1, PolyRest 2)
                             , (3, PolyRest 4)
                             , (12, PolyRest 4)])

    , poly (Polynome "y" [(1, PolyRest 2) ,(12, PolyRest 4)]) +
      poly (Polynome "x" [(1, PolyRest 2) ,(3, PolyRest 4)])
      ==> poly (Polynome "x" [ (0, Polynome "y" [(1, PolyRest 2) ,(12, PolyRest 4)])
                             , (1, PolyRest 2)
                             , (3, PolyRest 4)
                             ])

    , poly (Polynome "x" [(0, PolyRest 2), (12, PolyRest 4)]) +
      poly (Polynome "y" [(0, PolyRest 1), (1, PolyRest 2) ,(3, PolyRest 4)])
      ==> poly (Polynome "x" [ (0, Polynome "y" [(0, PolyRest 3), (1, PolyRest 2) ,(3, PolyRest 4)])
                             , (12, PolyRest 4)])

    , poly (Polynome "x" [ (0, PolyRest 2), (12, PolyRest 4)]) +
      poly (Polynome "y" [ (0, Polynome "z" [(1, PolyRest 1)])
                         , (1, PolyRest 2)
                         , (3, PolyRest 4) ])
      ==> poly (Polynome "x" [ (0, Polynome "y" [ (0, Polynome "z" [(0, PolyRest 2)
                                                                   ,(1, PolyRest 1)])
                                                , (1, PolyRest 2)
                                                , (3, PolyRest 4)])
                             , (12, PolyRest 4)])

    -- Substraction....
    , poly (Polynome "x" [(1, PolyRest 2) ,(3, PolyRest 4)]) -
      poly (Polynome "y" [(1, PolyRest 2) ,(3, PolyRest 4)])
      ==> poly (Polynome "x" [ (0,Polynome "y" [(1, PolyRest (-2)) ,(3, PolyRest (-4))])
                             , (1, PolyRest 2)
                             , (3, PolyRest 4)])

    , poly (Polynome "x" [(0, PolyRest 2), (1, PolyRest 2), (3, PolyRest 4), (12, PolyRest 4)]) -
      poly (Polynome "y" [(1, PolyRest 2) ,(3, PolyRest 4)])
      ==> poly (Polynome "x" [ (0, Polynome "y" [(0, PolyRest 2), (1, PolyRest (-2)) ,(3, PolyRest (-4))])
                             , (1, PolyRest 2)
                             , (3, PolyRest 4)
                             , (12, PolyRest 4)])

    , poly (Polynome "y" [(1, PolyRest 2) ,(12, PolyRest 4)]) -
      poly (Polynome "x" [(1, PolyRest 2) ,(3, PolyRest 4)])
      ==> poly (Polynome "x" [ (0, Polynome "y" [(1, PolyRest 2) ,(12, PolyRest 4)])
                             , (1, PolyRest (-2))
                             , (3, PolyRest (-4))
                             ])

    , poly (Polynome "x" [(0, PolyRest 2), (12, PolyRest 4)]) -
      poly (Polynome "y" [(0, PolyRest 1), (1, PolyRest 2) ,(3, PolyRest 4)])
      ==> poly (Polynome "x" [ (0, Polynome "y" [(0, PolyRest 1), (1, PolyRest (-2)) ,(3, PolyRest (-4))])
                             , (12, PolyRest 4)])

    , poly (Polynome "x" [ (0, PolyRest 2), (12, PolyRest 4)]) -
      poly (Polynome "y" [ (0, Polynome "z" [(1, PolyRest 1)])
                         , (1, PolyRest 2)
                         , (3, PolyRest 4) ])
      ==> poly (Polynome "x" [ (0, Polynome "y" [ (0, Polynome "z" [(0, PolyRest 2)
                                                                   ,(1, PolyRest (-1))])
                                                , (1, PolyRest (-2))
                                                , (3, PolyRest (-4))])
                             , (12, PolyRest 4)])

    -- Multiplication
    , poly (Polynome "x" [(0, PolyRest 1), (1, PolyRest 2), (2, PolyRest 3)]) *
      poly (Polynome "x" [(3, PolyRest 4), (5, PolyRest 2)])
      ==> poly (Polynome "x" [(3, PolyRest 4)
                             ,(4, PolyRest 8)
                             ,(5, PolyRest 14)
                             ,(6, PolyRest 4)
                             ,(7, PolyRest 6)
                             ])

    , poly (Polynome "x" [(0, PolyRest 1), (1, PolyRest 2), (2, PolyRest 3)]) *
      poly (Polynome "y" [(3, PolyRest 4), (5, PolyRest 2)])
      ==> poly (Polynome "x" [(0, Polynome "y" [(3, PolyRest 4), (5,PolyRest 2)])
                             ,(1, Polynome "y" [(3, PolyRest 8), (5,PolyRest 4)])
                             ,(2, Polynome "y" [(3, PolyRest 12), (5,PolyRest 6)])
                             ])

    -- Just like the last one, but swapped
    , poly (Polynome "y" [(3, PolyRest 4), (5, PolyRest 2)]) *
      poly (Polynome "x" [(0, PolyRest 1), (1, PolyRest 2), (2, PolyRest 3)])
      ==> poly (Polynome "x" [(0, Polynome "y" [(3, PolyRest 4), (5,PolyRest 2)])
                             ,(1, Polynome "y" [(3, PolyRest 8), (5,PolyRest 4)])
                             ,(2, Polynome "y" [(3, PolyRest 12), (5,PolyRest 6)])
                             ])

    --------------------------------------------------
    ----            Polynome/Variable
    --------------------------------------------------
    -- With Same variable
    -- *
    , var "x" * poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 4)]) ==>
        poly (Polynome "x" [(1, PolyRest 1) ,(2, PolyRest 2) ,(4, PolyRest 4)])

    , poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 4)]) * var "x" ==>
        poly (Polynome "x" [(1, PolyRest 1) ,(2, PolyRest 2) ,(4, PolyRest 4)])

    -- +
    , var "x" + poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 4)]) ==>
        poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 3) ,(3, PolyRest 4)])

    , poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 4)]) + var "x" ==>
        poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 3) ,(3, PolyRest 4)])

    , var "x" + poly (Polynome "x" [(0, PolyRest 1), (3, PolyRest 4)]) ==>
        poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 1) ,(3, PolyRest 4)])

    , poly (Polynome "x" [(0, PolyRest 1) ,(3, PolyRest 4)]) + var "x" ==>
        poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 1) ,(3, PolyRest 4)])

    -- -
    , var "x" - poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 4)]) ==>
        poly (Polynome "x" [(0, PolyRest (-1)), (1, PolyRest (-1)) ,(3, PolyRest (-4))])

    , poly (Polynome "x" [(0, PolyRest 2) ,(1, PolyRest 2) ,(3, PolyRest 4)]) - var "x" ==>
        poly (Polynome "x" [(0, PolyRest 2) ,(1, PolyRest 1) ,(3, PolyRest 4)])

    , var "x" - poly (Polynome "x" [(0, PolyRest 1) ,(3, PolyRest 4)]) ==>
        poly (Polynome "x" [(0, PolyRest (-1)), (1, PolyRest 1) ,(3, PolyRest (-4))])

    , poly (Polynome "x" [(0, PolyRest 2) ,(3, PolyRest 4)]) - var "x" ==>
        poly (Polynome "x" [(0, PolyRest 2) ,(1, PolyRest (-1)) ,(3, PolyRest 4)])

    -- With /= variable
    , var "y" * poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 4)]) ==>
        poly (Polynome "x" [ (0, Polynome "y" [(1, PolyRest 1)])
                           , (1, Polynome "y" [(1, PolyRest 2)])
                           , (3, Polynome "y" [(1, PolyRest 4)])
                           ])

    , poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 4)]) * var "y" ==>
        poly (Polynome "x" [ (0, Polynome "y" [(1, PolyRest 1)])
                           , (1, Polynome "y" [(1, PolyRest 2)])
                           , (3, Polynome "y" [(1, PolyRest 4)])
                           ])

    -- +
    , var "y" + poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 4)]) ==>
        poly (Polynome "x" [(0, Polynome "y" [(0, PolyRest 1), (1, PolyRest 1)])
                           ,(1, PolyRest 2) ,(3, PolyRest 4)])

    , poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 4)]) + var "y" ==>
        poly (Polynome "x" [(0, Polynome "y" [(0, PolyRest 1), (1, PolyRest 1)])
                           ,(1, PolyRest 2) ,(3, PolyRest 4)])

    , var "y" + poly (Polynome "x" [(0, PolyRest 1), (3, PolyRest 4)]) ==>
        poly (Polynome "x" [(0, Polynome "y" [(0, PolyRest 1), (1, PolyRest 1)])
                           ,(3, PolyRest 4)])

    , poly (Polynome "x" [(0, PolyRest 1) ,(3, PolyRest 4)]) + var "y" ==>
        poly (Polynome "x" [(0, Polynome "y" [(0, PolyRest 1), (1, PolyRest 1)])
                           ,(3, PolyRest 4)])

    -- -
    , var "y" - poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 4)]) ==>
        poly (Polynome "x" [ (0, Polynome "y" [(0, PolyRest (-1)), (1, PolyRest 1)])
                           , (1, PolyRest (-2)) ,(3, PolyRest (-4))])

    , poly (Polynome "x" [(0, PolyRest 2) ,(1, PolyRest 2) ,(3, PolyRest 4)]) - var "y" ==>
        poly (Polynome "x" [ (0, Polynome "y" [(0, PolyRest 2), (1, PolyRest (-1))])
                           , (1, PolyRest (2)) ,(3, PolyRest (4))])
    --------------------------------------------------
    ----            Polynome/Variable^n
    --------------------------------------------------
    , var "x" ** 2 * poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 4)]) ==>
        poly (Polynome "x" [(2, PolyRest 1) ,(3, PolyRest 2) ,(5, PolyRest 4)])

    , poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 4)]) * var "x" ** 2 ==>
        poly (Polynome "x" [(2, PolyRest 1) ,(3, PolyRest 2) ,(5, PolyRest 4)])

    -- +
    , var "x" ** 3 + poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 4)]) ==>
        poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 5)])

    , poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 4)]) + var "x" ** 2==>
        poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) , (2, PolyRest 1),(3, PolyRest 4)])

    , var "x" ** 2 + poly (Polynome "x" [(0, PolyRest 1), (3, PolyRest 4)]) ==>
        poly (Polynome "x" [(0, PolyRest 1) ,(2, PolyRest 1) ,(3, PolyRest 4)])

    , poly (Polynome "x" [(0, PolyRest 1) ,(3, PolyRest 4)]) + var "x" ** 2 ==>
        poly (Polynome "x" [(0, PolyRest 1) ,(2, PolyRest 1) ,(3, PolyRest 4)])

    -- -
    , var "x" ** 2 - poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 4)]) ==>
        poly (Polynome "x" [ (0, PolyRest (-1)), (1, PolyRest (-2))
                           , (2, PolyRest    1) ,(3, PolyRest (-4))])

    , poly (Polynome "x" [(0, PolyRest 2) ,(1, PolyRest 2) ,(3, PolyRest 4)]) - var "x" ** 2 ==>
        poly (Polynome "x" [(0, PolyRest 2) ,(1, PolyRest 2), (2, PolyRest (-1)), (3, PolyRest 4)])

    , var "x" ** 3 - poly (Polynome "x" [(0, PolyRest 1) ,(3, PolyRest 4)]) ==>
        poly (Polynome "x" [(0, PolyRest (-1)), (3, PolyRest (-3))])

    , poly (Polynome "x" [(0, PolyRest 2) ,(3, PolyRest 4)]) - var "x" ** 3 ==>
        poly (Polynome "x" [(0, PolyRest 2) ,(3, PolyRest 3)])

    -- With /= variable
    {-
    , var "y" * poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 4)]) ==>
        poly (Polynome "x" [ (0, Polynome "y" [(1, PolyRest 1)])
                           , (1, Polynome "y" [(1, PolyRest 2)])
                           , (3, Polynome "y" [(1, PolyRest 4)])
                           ])

    , poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 4)]) * var "y" ==>
        poly (Polynome "x" [ (0, Polynome "y" [(1, PolyRest 1)])
                           , (1, Polynome "y" [(1, PolyRest 2)])
                           , (3, Polynome "y" [(1, PolyRest 4)])
                           ])

    -- +
    , var "y" + poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 4)]) ==>
        poly (Polynome "x" [(0, Polynome "y" [(0, PolyRest 1), (1, PolyRest 1)])
                           ,(1, PolyRest 2) ,(3, PolyRest 4)])

    , poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 4)]) + var "y" ==>
        poly (Polynome "x" [(0, Polynome "y" [(0, PolyRest 1), (1, PolyRest 1)])
                           ,(1, PolyRest 2) ,(3, PolyRest 4)])

    , var "y" + poly (Polynome "x" [(0, PolyRest 1), (3, PolyRest 4)]) ==>
        poly (Polynome "x" [(0, Polynome "y" [(0, PolyRest 1), (1, PolyRest 1)])
                           ,(3, PolyRest 4)])

    , poly (Polynome "x" [(0, PolyRest 1) ,(3, PolyRest 4)]) + var "y" ==>
        poly (Polynome "x" [(0, Polynome "y" [(0, PolyRest 1), (1, PolyRest 1)])
                           ,(3, PolyRest 4)])

    -- -
    , var "y" - poly (Polynome "x" [(0, PolyRest 1) ,(1, PolyRest 2) ,(3, PolyRest 4)]) ==>
        poly (Polynome "x" [ (0, Polynome "y" [(0, PolyRest (-1)), (1, PolyRest 1)])
                           , (1, PolyRest (-2)) ,(3, PolyRest (-4))])

    , poly (Polynome "x" [(0, PolyRest 2) ,(1, PolyRest 2) ,(3, PolyRest 4)]) - var "y" ==>
        poly (Polynome "x" [ (0, Polynome "y" [(0, PolyRest 2), (1, PolyRest (-1))])
                           , (1, PolyRest (2)) ,(3, PolyRest (4))])
             -}
    ]

comparisonOperator :: [(FormulaPrim, FormulaPrim)]
comparisonOperator =
    [ binOp OpLt [1, 2, 3, 4, 5, 8, 10] ==> Truth True
    , binOp OpLt [1, 2, 3, 3, 5, 8, 10] ==> Truth False
    , binOp OpGt [1, 2, 3, 4, 5, 8, 10] ==> Truth False
    , binOp OpGe [1, 2, 3, 4, 5, 8, 10] ==> Truth False

    , binOp OpLe [1, 2, 3, 3, 5, 8, 10] ==> Truth True
    , binOp OpLe [1, 2, 3, 4, 5, 8, 10] ==> Truth True

    , binOp OpGt [10, 8, 5, 4, 3, 2, 1] ==> Truth True
    , binOp OpLt [10, 8, 5, 4, 3, 2, 1] ==> Truth False
    , binOp OpLe [10, 8, 5, 4, 3, 2, 1] ==> Truth False

    , binOp OpGt [10, 8, 5, 5, 3, 2, 1] ==> Truth False
    , binOp OpGe [10, 8, 5, 5, 3, 2, 1] ==> Truth True

    , binOp OpGt [10, 8, Variable "x", 4, 3, 2, 1]
      ==> binOp OpGt [8, Variable "x", 4]
    , binOp OpLt [1, 2, 3, 4, Variable "x", 8, 10]
      ==> binOp OpLt [4, Variable "x", 8]

    , binOp OpEq [1, 2, 3, 4, 5, 8, 10] ==> Truth False
    , binOp OpEq [1, 1, 1, 1] ==> Truth True
    , binOp OpNe [1, 1, 1, 1] ==> Truth False
    , binOp OpEq [1, 1, 1, CFloat 1.0] ==> Truth True

    , binOp OpEq [1, Variable "x", 1, 1] ==> binOp OpEq [Variable "x", 1]
    , binOp OpEq [1, Variable "x", 1, 2] ==> Truth False
    ]

basicManualFunction :: [(FormulaPrim, FormulaPrim)]
basicManualFunction =
    [ unOp OpNegate (int 12) ==> unOp OpNegate (int 12)
    , unOp OpNegate (float 12) ==> float (-12)
    , unOp OpNegate (unOp OpNegate $ int 12) ==> int 12
    , unOp OpNegate (unOp OpNegate $ float 12) ==> float 12

    , unOp OpNegate (unOp OpNegate $ Variable "x") ==> Variable "x"

    , unOp OpAbs (unOp OpNegate $ int 12) ==> int 12
    , unOp OpAbs (float (-12)) ==> float 12

    , unOp OpFrac (int 4) ==> int 0
    , unOp OpFrac (int 12) ==> int 0
    , unOp OpFrac (float 12.0) ==> int 0
    , unOp OpFrac (float 12.5) ==> float 0.5

    , unOp OpASin (float 0.5) ==> CFloat (asin 0.5)
    , unOp OpASin (int 0) ==> zeroFy (CFloat $ asin 0)

    , unOp OpACos (float 0.5) ==> CFloat (acos 0.5)
    , unOp OpACos (int 0) ==> CFloat (acos 0)

    , unOp OpATanh (float 0.5) ==> zeroFy (CFloat $ atanh 0.5)
    , unOp OpATanh (int 0) ==> zeroFy (CFloat $ atanh 0)

    , unOp OpASinh (float 0.5) ==> CFloat (asinh 0.5)
    , unOp OpASinh (int 0) ==> zeroFy (CFloat $ asinh 0)

    , unOp OpACosh (float 1.5) ==> CFloat (acosh 1.5)
    , unOp OpACosh (int 1) ==> zeroFy (CFloat $ acosh 1)
    ]

errorFormula :: FormulaPrim
errorFormula = Block 1 1 1

listCreationOperation :: [(FormulaPrim, FormulaPrim)]
listCreationOperation =
    [ binOp OpCons [2, 3]          ==> errorFormula
    , binOp OpCons [2, 3, list []] ==> list [2,3]
    , binOp OpCons [1,
        binOp OpCons [2,
            binOp OpCons [3, list [] ]]] ==> list [1,2,3]
    ]

indexationOperation :: [(FormulaPrim, FormulaPrim)]
indexationOperation =
    [ indexes (int 3) [int 1]       ==> errorFormula
    , indexes (list [1, 2, 3, 4, 5, 6, 7, 9, 10]) [4] ==> 4
    , indexes (matrix 3 3 [ [ 1, 2, 3 ]
                          , [ 4, 5, 6 ]
                          , [ 7, 8, 9 ] ]) [1, 2] ==> 2

    , indexes (matrix 3 3 [ [ 1, 2, 3 ]
                          , [ 4, 5, 6 ]
                          , [ 7, 8, 9 ] ]) [2, 3] ==> 6

    , indexes (matrix 3 3 [ [ 1, 2, 3 ]
                          , [ 4, 5, 6 ]
                          , [ 7, 8, 9 ] ]) [1, 1] ==> 1

    , indexes (matrix 3 3 [ [ 1, 2, 3 ]
                          , [ 4, 5, 6 ]
                          , [ 7, 8, 9 ] ]) [1, 4] ==> errorFormula

    , indexes (matrix 3 3 [ [ 1, 2, 3 ]
                          , [ 4, 5, 6 ]
                          , [ 7, 8, 9 ] ]) [4, 2] ==> errorFormula

    , indexes (matrix 3 3 [ [ 1, 2, 3 ]
                          , [ 4, 5, 6 ]
                          , [ 7, 8, 9 ] ]) [0, 0] ==> errorFormula

    , indexes (matrix 2 2 [ [ 1, 2            ]
                          , [ 4, Variable "a" ] ]) [2, 2, 3] ==> indexes (Variable "a") [3]
    , indexes (matrix 5 1 [ [ 1, 2, 3, 4, 5 ] ]) [3] ==> 3
    , indexes (matrix 1 5 [ [1], [2], [3], [4], [5] ]) [4] ==> 4
    , indexes (matrix 5 1 [ [ 1, 2, 3, 4, 5 ] ]) [7] ==> errorFormula
    , indexes (matrix 1 5 [ [1], [2], [3], [4], [5] ]) [6] ==> errorFormula
    , indexes (matrix 5 1 [ [ 1, 2, 3, 4, 5 ] ]) [0] ==> errorFormula
    , indexes (matrix 1 5 [ [1], [2], [3], [4], [5] ]) [0] ==> errorFormula
    ]

basicFunctions :: FormulaPrim -> Double -> [(FormulaPrim, FormulaPrim)]
basicFunctions n floatN =
    [ unOp OpSqrt n ==> CFloat (sqrt floatN)
    , unOp OpSin n ==> CFloat (sin floatN)
    , unOp OpSinh n ==> CFloat (sinh floatN)
    , unOp OpCos n ==> CFloat (cos floatN)
    , unOp OpCosh n ==> CFloat (cosh floatN)

    , unOp OpTan n ==> CFloat (tan floatN)
    , unOp OpTanh n ==> CFloat (tanh floatN)
    , unOp OpATan n ==> CFloat (atan floatN)
    , unOp OpLn n ==> zeroFy (CFloat $ log floatN)
    , unOp OpLog n ==> zeroFy (CFloat $ log floatN / log 10.0)
    , unOp OpExp n ==> CFloat (exp floatN)
    -- ?)
    --, unOp OpCeil n ==> CInteger (ceil floatN)
    , unOp OpFloor n ==> CInteger (floor floatN)
    ]

coeffArithmeticTest :: [(PolyCoeff, PolyCoeff)]
coeffArithmeticTest =
    let cint = CoeffInt
        cfloat = CoeffFloat
        cratio = CoeffRatio
    in
    [ cint 2 + cint 3 ==> cint 5
    , cint 2 - cint 3 ==> cint (-1)
    , cint 2 * cint 3 ==> cint 6
    , cint 2 / cint 3 ==> cratio (2 % 3)

    , cfloat 2 + cfloat 3 ==> cfloat 5
    , cfloat 2 - cfloat 3 ==> cfloat (-1)
    , cfloat 2 * cfloat 3 ==> cfloat 6
    , cfloat 2 / cfloat 3 ==> cfloat (2.0 / 3.0)

    , cint 1 ==> cfloat 1
    ]

evalResultCheck :: FormulaPrim -> FormulaPrim -> Assertion
evalResultCheck toEval finalFormula = assertEqual "" finalFormula
                                    . unTagFormula
                                    . result
                                    . performTransformation
                                    . evalGlobalLossyStatement
                                    . listifyFormula 
                                    $ Formula toEval

exactevalResultCheck :: FormulaPrim -> FormulaPrim -> Assertion
exactevalResultCheck toEval finalFormula = assertEqual "" finalFormula
                                        . unTagFormula
                                        . result
                                        . performTransformation
                                        . evalGlobalLosslessStatement
                                        . listifyFormula 
                                        $ Formula toEval

zeroFy :: FormulaPrim -> FormulaPrim
zeroFy (CFloat 0.0) = CInteger 0
zeroFy a = a

wellFormated :: Formula TreeForm -> Bool
wellFormated f = latexRender defaultRenderConf f /= ""
              && formatFormula defaultRenderConf f /= ""
              && mathmlRender defaultRenderConf f /= ""
              && sexprRender f /= ""

formatTester :: String -> IO Bool
formatTester filename = do
    formulaText <- readFile filename
    either (const $ return False)
           (return . wellFormated . treeIfyFormula)
           $ perfectParse formulaText

folder :: FilePath
folder = "tests" </> "format"

formatTestList :: IO [Test]
formatTestList = pathes >>= mapM (\f -> return $ formatTester f ~? f)
        where pathes :: IO [FilePath]
              pathes = map (folder </>) . filter (\f -> takeExtension f == ".txt")
                    <$> getDirectoryContents folder

