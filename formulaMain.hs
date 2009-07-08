import System.Environment
import FormulaTypes
import FormulaLinker
import AsciiRenderer
import CharArray
import Data.List( intersperse )
import Text.ParserCombinators.Parsec.Prim( runParser )

main :: IO ()
main = do
    args <- getArgs
    let input = args !! 0
        output = args !! 1
        write = writeFile output
    formulaText <- readFile input
    let formula = runParser expr () "FromFile" formulaText
    either (\_ -> print "No print-out")
           (\unlinkedFormula ->
                  let f = linkFormula unlinkedFormula
                      (f', tree) = renderFormula f
                      formulaMatrix = linesOfArray f'
                  in
                  do write . concat $ intersperse "\n" formulaMatrix
                     writeFile "formula.txt" $ show formula
                     writeFile "size.txt" $ show tree)
           formula

