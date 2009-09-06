import System.Environment
import Text.Printf


import EqManips.Types
import EqManips.Linker
import EqManips.Propreties
import EqManips.FormulaIterator
import EqManips.EvaluationContext
import EqManips.Algorithm.Cleanup
import EqManips.Algorithm.Utils
import EqManips.Algorithm.Expand
import EqManips.Algorithm.Eval
import EqManips.Algorithm.Cleanup
import EqManips.Algorithm.EmptyMonad

import EqManips.Tests.FullGenerator()
import EqManips.Tests.ContinuousGenerator()

import Test.QuickCheck
{-import Test.QuickCheck.Batch-}

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

preserveMeaning :: (Formula -> Formula) -> Formula -> Bool
preserveMeaning transformation f = 
  not (iniVal `hasProp` LeafNode) || comp (clean iniVal)
                                          (clean . eval $ transformation f)
    where eval = result . performTransformation . reduce
          iniVal = eval f
          clean (CFloat 0.0) = CInteger 0
          clean (CFloat 1.0) = CInteger 1
          clean (CFloat (-1.0)) = CInteger (-1)
          clean e = e

          comp (CFloat f1) (CFloat f2)
            | isNaN f1 = isNaN f2
            | otherwise = f1 == f2
          comp a b = a == b

testRunner :: Testable a => a -> Int -> IO ()
testRunner prop count = check config prop
    where config = defaultConfig { configMaxTest = count
                                 , configMaxFail = 2 }

prop_treelistify :: Formula -> Bool
prop_treelistify f = listifyFormula f == listifyFormula (treeIfyFormula f)

globalTests :: [(String, Int -> IO ())]
globalTests =
    [ ("Formula deparsing", testRunner prop_showBack)
    , ("Formula ordering", testRunner prop_ordering)
    , ("Formula tree/list", testRunner prop_treelistify) 
    , ("Formula folding", testRunner prop_nodeCount)
    , ("Formula depth first traversal", testRunner prop_depthFirstFormula)
    , ("Expand don't change meaning", testRunner $ preserveMeaning expand)
    , ("Treeify don't change meaning", testRunner $ preserveMeaning treeIfyFormula)
    , ("Listify don't change meaning", testRunner $ preserveMeaning listifyFormula)
    , ("Cleanup don't change meaning", testRunner $ preserveMeaning cleanup)
    ]

runTestList :: [(String, Int -> IO ())] -> IO ()
runTestList tests = do
    x <- getArgs
    let n = if null x then 100 else read . head $ x
    mapM_ (\(s,a) -> printf "%-25s: " s >> a n) tests

main :: IO ()
main = runTestList globalTests

