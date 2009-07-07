import System.Environment
import FormulaTypes
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
           (\f -> let (f', tree) = renderFormula f
                      formulaMatrix = linesOfArray f'
                  in
                  write $ show formula ++ "\n\n\n"
                       ++ show tree
                       ++ "\n\n\n"
                       ++ (concat $ intersperse "\n" formulaMatrix))
           formula

