import EqManips.Tests.FullGenerator()
import EqManips.Tests.ContinuousGenerator()
import Test.QuickCheck
import EqManips.Types
import EqManips.Linker
import EqManips.Algorithm.Cleanup
import EqManips.Algorithm.Utils
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

main :: IO ()
main = do
    quickCheck prop_showBack
    quickCheck prop_ordering
    quickCheck prop_nodeCount

