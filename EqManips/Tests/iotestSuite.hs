import EqManips.Tests.FullGenerator
import Control.Monad
import Test.QuickCheck
import EqManips.Types
import EqManips.EvaluationContext
import EqManips.Linker
import EqManips.Algorithm.Cleanup
import Text.ParserCombinators.Parsec.Prim( runParser )

prop_showBack formula = case eitherformula of
             Left _ -> False
             Right f -> let f' = performTransformation . cleanup $ linkFormula f
                            formula' = performTransformation $ cleanup formula
                        in result f' == result formula'
    where text = unparse formula
          eitherformula = runParser expr () "FromFile" text

main = do
    quickCheck prop_showBack
    quickCheck prop_showBack
    quickCheck prop_showBack
    quickCheck prop_showBack
    quickCheck prop_showBack
