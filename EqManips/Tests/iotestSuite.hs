import EqManips.Tests.FullGenerator()
import EqManips.Tests.ContinuousGenerator()
import Test.QuickCheck
import EqManips.Types
import EqManips.Linker
import EqManips.Algorithm.Cleanup
import Text.ParserCombinators.Parsec.Prim( runParser )

prop_showBack :: Formula -> Bool
prop_showBack formula = case eitherformula of
             Left _ -> False
             Right f -> 
                (cleanup $ linkFormula f) == cleanup formula
    where text = unparse formula
          eitherformula = runParser expr () "FromFile" text

main :: IO ()
main = do
    quickCheck prop_showBack

