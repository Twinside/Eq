module EqManips.Tests.UnitTest( eqUnittests, runEqTests ) where

import Data.Ratio

import Test.HUnit
import Test.HUnit.Text
import EqManips.Types
import EqManips.EvaluationContext
import EqManips.Algorithm.Utils
import EqManips.Algorithm.Eval
import EqManips.Renderer.Sexpr

infixr 1 ==>

int :: Integer -> FormulaPrim
int = CInteger

float :: Double -> FormulaPrim
float = CFloat

frac :: (Integer, Integer) -> FormulaPrim
frac (a,b) = Fraction $ a % b

runEqTests :: IO Bool
runEqTests = do
    rez <- runTestTT eqUnittests
    return $ errors rez == 0 && failures rez == 0

eqUnittests :: Test
eqUnittests = TestList $
    [ TestLabel (sexprRender (Formula $ binOp OpEq [toEval, rez]) ++ " ")
    . TestCase
    $ evalResultCheck toEval rez | (toEval, rez) <- arithmeticTest ]

(==>) :: a -> b -> (a,b)
(==>) = (,)

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
    , complex (int 1, int 2) - float 1 ==> complex (float 0, int 2)
    , complex (int 4, int 2) * float 2 ==> complex (float 8, float 4)
    , complex (int 4, int 2) / float 2 ==> complex (float 2, float 1)

    -- Complex -> Complex -> Complex
    , complex (int 0, int 1) + complex (int 1, int 2) ==> complex (int 1, int 3)
    , complex (int 2, int 3) - complex (int 1, int 2) ==> complex (int 1, int 1)
    , complex (int 0, int 1) * complex (int 1, int 2) ==> complex (negate $ int 2, int 1)
    , complex (int 0, int 4) / complex (int 1, int 2) ==> complex (frac (8,5), frac (4,5))

    -- Fraction -> Fraction -> Fraction
    , frac (1,2) + frac (1,2) ==> int 1
    , frac (1,2) + frac (1,4) ==> frac (3,4)

    , frac (1,2) - frac (1,2) ==> int 0
    , frac (1,2) - frac (1,4) ==> frac (1,4)

    , frac (1,3)   * frac (1, 4) ==> frac (1,12)
    , frac (-1, 4) * frac (2, 4) ==> frac (-1, 8)

    , frac (1,3)   / frac (1, 4) ==> frac (4,3)
    , frac (-1, 4) / frac (2, 4) ==> frac (-1, 2)
    ]

evalResultCheck :: FormulaPrim -> FormulaPrim -> Assertion
evalResultCheck toEval finalFormula = assertEqual "" finalFormula
                                    . unTagFormula
                                    . result
                                    . performTransformation
                                    . evalGlobalLossyStatement
                                    . listifyFormula 
                                    $ Formula toEval

