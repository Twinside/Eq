import System.Environment
import EqManips.Types
import EqManips.Linker
import EqManips.Renderer.Ascii
import CharArray
import Data.List( intersperse )
import Text.ParserCombinators.Parsec.Prim( runParser )

import System.IO
import System.Console.GetOpt

import Data.Maybe( fromMaybe )


-- Just to be able to compile...
import EqManips.Algorithm.Eval
import EqManips.EvaluationContext

data Flag =
    Output
    deriving (Eq, Show)

version :: String
version = "0.1"

formatFormula :: Formula -> String
formatFormula f = concat $ intersperse "\n" formulaMatrix
    where (f', _tree) = renderFormula f
          formulaMatrix = linesOfArray f'

commonOption :: [OptDescr (Flag, String)]
commonOption =
    [ Option ['o']  ["output"] (ReqArg ((,) Output) "FILE") "output FILE" ]

formatOption :: [OptDescr (Flag, String)]
formatOption = commonOption

formatCommand :: [String] -> IO ()
formatCommand args = do
    formulaText <- readFile input
    let formula = runParser expr () "FromFile" formulaText
    output <- outputFile
    either (\err -> print "Error : " >> print err)
           (\formula' -> hPutStrLn output. formatFormula $ linkFormula formula')
           formula
    hClose output

     where (opt, left, _) = getOpt Permute formatOption args
           input = left !! 0
           outputFile = case lookup Output opt of
                             Nothing -> return stdout 
                             Just n -> openFile n ReadMode

transformParseFormula :: (Formula -> EqContext Formula) -> [String]
                      -> IO ()
transformParseFormula operation args = do
    formulaText <- readFile input
    let formula = runParser expr () "FromFile" formulaText
    either (\err -> print "Error : " >> print err)
           (\formula' -> do
               let rez = performTransformation 
                                . operation 
                                $ linkFormula formula'
               mapM_ (\(f,s) -> do
                            putStrLn s
                            putStrLn $ formatFormula f) $ errorList rez
               write . formatFormula $ result rez)
           formula

     where (opt, left, _) = getOpt Permute formatOption args
           input = left !! 0
           output = fromMaybe "stdout" $ lookup Output opt
           write = writeFile output

helpCommand :: [String] -> IO ()
helpCommand [] = do
    putStrLn $ "EqManips " ++ version ++ " command list"
    putStrLn ""
    mapM_ printCommand commandList
    putStrLn ""
    where maxCommandLen = 4 + maximum [ length c | (c,_,_) <- commandList ]
          spaces = repeat ' '
          printCommand (com, hlp, _) = do
              putStrLn $ ' ' : com 
                      ++ take (maxCommandLen - length com) spaces 
                      ++ hlp
helpCommand _args = return ()

commandList :: [(String, String, [String] -> IO ())]
commandList = 
    [ ("cleanup", "Perform trivial simplification on formula", transformParseFormula reduce)
    , ("eval", "Try to evaluate/reduce the formula", transformParseFormula reduce)
    , ("format", "Load and display the formula in ASCII Art", formatCommand)
    , ("help", "Ask specific help for a command, or this", helpCommand)
    ]

reducedCommand :: [(String, [String] -> IO ())]
reducedCommand = map (\(n,_,a) -> (n,a)) commandList

main :: IO ()
main = do
    args <- getArgs
    if null args
       then error "No command give, try the help command"
       else case lookup (args !! 0) reducedCommand of
                 Just c -> c $ tail args
                 Nothing -> error $ "Unknown command " ++ (args !! 0)
              
