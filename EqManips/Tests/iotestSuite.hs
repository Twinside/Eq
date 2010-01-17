import System.Environment
import Text.Printf
import Data.Monoid( All( .. ), mempty, mappend )

import Control.Monad

import EqManips.Types
import EqManips.Linker
import EqManips.Propreties
import EqManips.FormulaIterator
import EqManips.EvaluationContext
import qualified EqManips.Algorithm.Cleanup as Cleanup
import EqManips.Algorithm.Utils
import EqManips.Algorithm.Expand
import EqManips.Algorithm.Eval
import EqManips.Algorithm.EmptyMonad
import EqManips.InputParser.EqCode
import EqManips.Renderer.EqCode
import EqManips.Tests.FullGenerator()
import EqManips.Tests.ContinuousGenerator()

import Test.QuickCheck
{-import Test.QuickCheck.Batch-}

import Text.ParserCombinators.Parsec.Prim( runParser )
import EqManips.Tests.UnitTest

cleanup :: FormulaPrim -> FormulaPrim
cleanup = unTagFormula . Cleanup.cleanup . Formula

-----------------------------------------------
----        Helpers
-----------------------------------------------
preserveMeaning :: (FormulaPrim -> FormulaPrim) -> FormulaPrim -> Bool
preserveMeaning transformation f = 
  not (iniVal `hasProp` LeafNode) || comp (clean iniVal)
                                          (clean . eval $ transformation f)
    where eval = unTagFormula . result . performTransformation . reduce . Formula
          iniVal = eval f
          clean (CFloat 0.0) = CInteger 0
          clean (CFloat 1.0) = CInteger 1
          clean (CFloat (-1.0)) = CInteger (-1)
          clean e = e

          comp (CFloat f1) (CFloat f2)
            | isNaN f1 = isNaN f2
            | otherwise = f1 == f2
          comp a b = a == b

mustVerify :: (FormulaPrim -> Bool) -> FormulaPrim -> Bool
mustVerify f = getAll . foldf combiner mempty
    where combiner formula = (`mappend` (All $ f formula))

-----------------------------------------------
----        Propreties
-----------------------------------------------
prop_showBack :: FormulaPrim -> Bool
prop_showBack formula = case eitherformula of
             Left _ -> False
             Right f -> 
                (Cleanup.cleanup . linkFormula $ Formula f) ==
                    (Cleanup.cleanup . listifyFormula $ Formula formula)
    where text = unparse formula
          eitherformula = runParser expr () "FromFile" text

prop_ordering :: FormulaPrim -> Bool
prop_ordering f = f <= f

prop_nodeCount :: FormulaPrim -> Bool
prop_nodeCount f = nodeCount f > 0

prop_depthFirstFormula :: FormulaPrim -> Bool
prop_depthFirstFormula f = (depthFormulaPrimTraversal `asAMonad` id $ f) == f

prop_treelistify :: FormulaPrim -> Bool
prop_treelistify f = listOnce == treeList 
    where listOnce = unTagFormula . listifyFormula $ Formula f
          treeList = unTagFormula . listifyFormula . treeIfyFormula $ Formula f

prop_treefi2 :: FormulaPrim -> Bool
prop_treefi2 f = mustVerify size2 . unTagFormula . treeIfyFormula $ Formula f
    where size2 (BinOp _ _op lst) = length lst == 2
          size2 _ = True

-----------------------------------------------
----        Test list.
-----------------------------------------------
globalTests :: [(String, Bool -> String -> Int -> IO ())]
globalTests =
    [ ("FormulaPrim ordering", testRunner prop_ordering)
    , ("FormulaPrim tree/list", testRunner prop_treelistify)
    , ("FormulaPrim folding", testRunner prop_nodeCount)
    , ("FormulaPrim depth first traversal", testRunner prop_depthFirstFormula)
    , ("Expand don't change meaning", testRunner . preserveMeaning $ unTagFormula 
                                                                   . expand
                                                                   . treeIfyFormula
                                                                   . Formula)
    -- Well the next case is... well.. a bit stupid. We don't evaluate formula
    -- in tree form, wich allow us to do a better evaluation (quicker, based
    -- on less actual bound variables), and everything is listified which, is
    -- already tested =)
    -- Keep code here in case of mind changing (screw git)
    {-
    , ("Treeify don't change meaning", testRunner . preserveMeaning $ unTagFormula 
                                                                    . treeIfyFormula
                                                                    . Formula)
     -}
    , ("Listify don't change meaning", testRunner . preserveMeaning $ unTagFormula 
                                                                    . listifyFormula
                                                                    . Formula)
    , ("Cleanup don't change meaning", testRunner $ preserveMeaning cleanup)
    , ("Treeify has BinOp of size 2", testRunner prop_treefi2)

    -- Not satisfiable
    --, ("FormulaPrim deparsing", testRunner prop_showBack)
    ]

-----------------------------------------------
----        System
-----------------------------------------------
testRunner :: Testable a => a -> Bool -> String -> Int -> IO ()
testRunner prop _verbose _txt count = quickCheckWith config prop
    where config = stdArgs { maxSuccess = count
                           , maxDiscard = 2 
                           }
          {-
          display = if verbose
                then \n args -> txt ++ " " ++ show n ++ ":\n" ++ unlines args
                else \n _ -> let s = show n in s ++ [ '\b' | _ <- s ]
                                 -}

parseArgs :: (Bool, Int) -> [String] -> (Bool, Int)
parseArgs params [] = params
parseArgs (_, n) ("-v":xs) = parseArgs (True, n) xs
parseArgs (v, _) (n:xs) = parseArgs (v, read n) xs

runTestList :: [(String, Bool -> String -> Int -> IO ())] -> IO ()
runTestList tests = do
    args <- getArgs
    let (verbose, n) = parseArgs (False, 100) args
    mapM_ (\(s,a) -> printf "%-25s: " s >> a verbose s n) tests

main :: IO ()
main = do
    valid <- runEqTests
    --when valid $ runTestList globalTests
    runTestList globalTests


