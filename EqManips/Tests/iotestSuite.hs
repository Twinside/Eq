import EqManips.Tests.FullGenerator()
import EqManips.Tests.ContinuousGenerator()
import Test.QuickCheck
import Test.QuickCheck.Batch
import System.Environment
import Text.Printf
import EqManips.Types
import EqManips.Linker
import EqManips.Algorithm.Cleanup
import EqManips.Algorithm.Utils
import EqManips.FormulaIterator
import EqManips.Algorithm.EmptyMonad
import Text.ParserCombinators.Parsec.Prim( runParser )

prop_showBack :: Formula -> Bool
prop_showBack formula = case eitherformula of
             Left _ -> False
             Right f -> 
                (cleanup $ linkFormula f) == cleanup formula
    where text = unparse formula
          eitherformula = runParser expr () "FromFile" text

prop_ordering :: Formula -> Bool
prop_ordering f = f <= f

prop_nodeCount :: Formula -> Bool
prop_nodeCount f = nodeCount f > 0

prop_depthFirstFormula :: Formula -> Bool
prop_depthFirstFormula f = (depthFirstFormula `asAMonad` id $ f) == f

testRunner :: Testable a => a -> Int -> IO ()
testRunner prop count = check config prop
    where config = defaultConfig { configMaxTest = count
                                 , configMaxFail = 2 }

globalTests :: [(String, Int -> IO ())]
globalTests =
    [ ("Formula deparsing", testRunner prop_showBack)
    , ("Formula ordering", testRunner prop_ordering)
    , ("Formula folding", testRunner prop_nodeCount)
    , ("Formula depth first traversal", testRunner prop_depthFirstFormula)
    ]

runTestList :: [(String, Int -> IO ())] -> IO ()
runTestList tests = do
    x <- getArgs
    let n = if null x then 100 else read . head $ x
    mapM_ (\(s,a) -> printf "%-25s:\n " s >> a n) tests

main :: IO ()
main = runTestList globalTests

