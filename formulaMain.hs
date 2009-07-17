import System.Environment
import EqManips.Types
import EqManips.Linker
import EqManips.Renderer.Ascii
import CharArray
import Data.List( intersperse )
import Text.ParserCombinators.Parsec.Prim( runParser )

-- Just to be able to compile...
import EqManips.Aproximated
import EqManips.Algorithm.Derivative

main :: IO ()
main = do
    args <- getArgs
    let input = args !! 0
        output = args !! 1
        write = writeFile output
    formulaText <- readFile input
    let formula = runParser expr () "FromFile" formulaText
    either (\err -> print "Error : " >> print err)
           (\unlinkedFormula ->
                  let f = linkFormula unlinkedFormula
                      (f', tree) = renderFormula f
                      formulaMatrix = linesOfArray f'
                  in
                  do write . concat $ intersperse "\n" formulaMatrix
                     writeFile "formula.txt" $ show f
                     writeFile "size.txt" $ show tree)
           formula

