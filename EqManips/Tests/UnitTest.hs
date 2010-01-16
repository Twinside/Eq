module EqManips.Tests.UnitTest( eqUnittests, runEqTests ) where

import Test.HUnit
import Test.HUnit.Text
import EqManips.Types
import EqManips.EvaluationContext
import EqManips.Algorithm.Utils
import EqManips.Algorithm.Eval
import EqManips.Renderer.Sexpr

int :: Integer -> FormulaPrim
int = CInteger

float :: Double -> FormulaPrim
float = CFloat

runEqTests :: IO Bool
runEqTests = do
    rez <- runTestTT eqUnittests
    return $ errors rez == 0 && failures rez == 0

eqUnittests :: Test
eqUnittests = TestList $
    [ TestLabel (sexprRender (Formula $ binOp OpEq [toEval, rez]) ++ " ")
    . TestCase
    $ evalResultCheck toEval rez | (toEval, rez) <- arithmeticTest ]

arithmeticTest :: [(FormulaPrim, FormulaPrim)]
arithmeticTest =
    [ (int 2 + int 3, int 5)
    , (int 2 * int 3, int 6)
    , (int 2 - int 3, int (-1))
    , (int 2 / int 3, float (2.0 / 3.0))

    , (float 2 + int 3, float 5)
    , (float 2 * int 3, float 6)
    , (float 2 - int 3, float (-1))
    , (float 2 / int 3, float (2.0 / 3.0))


    , (int 2 + float 3, float 5)
    , (int 2 * float 3, float 6)
    , (int 2 - float 3, float (-1))
    , (int 2 / float 3, float (2.0 / 3.0))

    , (complex (int 0, int 2) + int 1, complex (int 1, int 2))
    , (complex (int 1, int 2) - int 1, complex (int 0, int 2))
    , (complex (int 4, int 2) * int 2, complex (int 8, int 4))
    , (complex (int 4, int 2) / int 2, complex (int 2, int 1))

    , (complex (int 0, int 2) + float 1, complex (float 1, int 2))
    , (complex (int 1, int 2) - float 1, complex (float 0, int 2))
    , (complex (int 4, int 2) * float 2, complex (float 8, float 4))
    , (complex (int 4, int 2) / float 2, complex (float 2, float 1))

    , (complex (int 0, int 1) + complex (int 1, int 2), complex (int 1, int 3))
    , (complex (int 2, int 3) - complex (int 1, int 2), complex (int 1, int 1))
    , (complex (int 0, int 1) * complex (int 1, int 2), complex (negate $ int 2, int 1))
    , (complex (int 0, int 4) / complex (int 1, int 2), complex (int 0, int 2))
    ]

evalResultCheck :: FormulaPrim -> FormulaPrim -> Assertion
evalResultCheck toEval finalFormula = assertEqual "" finalFormula
                                    . unTagFormula
                                    . result
                                    . performTransformation
                                    . evalGlobalLossyStatement
                                    . listifyFormula 
                                    $ Formula toEval

