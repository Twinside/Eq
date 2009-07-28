import System.Environment
import EqManips.Types
import EqManips.Linker
import EqManips.Renderer.Ascii
import CharArray
import Data.List( intersperse )
import Text.ParserCombinators.Parsec.Prim( runParser )

import System.IO
import System.Console.GetOpt

import Data.List( find )

-- Just to be able to compile...
import EqManips.Algorithm.Eval
import EqManips.EvaluationContext

data Flag =
      Output
    | Input
    deriving Eq

version :: String
version = "0.1"

formatFormula :: Formula -> String
formatFormula f = concat $ intersperse "\n" formulaMatrix
    where (f', _tree) = renderFormula f
          formulaMatrix = linesOfArray f'

commonOption :: [OptDescr (Flag, String)]
commonOption =
    [ Option ['o']  ["output"] (ReqArg ((,) Output) "FILE") "output FILE"
    , Option ['f']  ["file"] (ReqArg ((,) Input) "FILE") "input FILE"
    ]

formatOption :: [OptDescr (Flag, String)]
formatOption = commonOption

getInputOutput :: [(Flag, String)] -> [String] -> (IO String, IO Handle)
getInputOutput opts args = (inputFile, outputFile)
   where outputFile = maybe (return stdout) (\name -> openFile name WriteMode)
                            (lookup Output opts)
         inputFile = maybe (return $ args !! 0) readFile
                           (lookup Input opts)

formatCommand :: [String] -> IO ()
formatCommand args = do
    formulaText <- input
    let formula = runParser expr () "FromFile" formulaText
    output <- outputFile
    either (\err -> print "Error : " >> print err)
           (\formula' -> hPutStrLn output. formatFormula $ linkFormula formula')
           formula
    hClose output

     where (opt, left, _) = getOpt Permute formatOption args
           (input, outputFile) = getInputOutput opt left

transformParseFormula :: (Formula -> EqContext Formula) -> [String]
                      -> IO ()
transformParseFormula operation args = do
    formulaText <- input
    finalFile <- outputFile
    let formula = runParser expr () "FromFile" formulaText
    either (\err -> print "Error : " >> print err)
           (\formula' -> do
               let rez = performTransformation 
                                . operation 
                                $ linkFormula formula'
               mapM_ (\(f,s) -> do
                            putStrLn s
                            putStrLn $ formatFormula f) $ errorList rez
               hPutStr finalFile . formatFormula $ result rez)
           formula

     where (opt, left, _) = getOpt Permute formatOption args
           (input, outputFile) = getInputOutput opt left

printVer :: IO ()
printVer = 
    putStrLn $ "EqManips " ++ version ++ " command list"

helpCommand :: [String] -> IO ()
helpCommand [] = do
    printVer
    putStrLn ""
    mapM_ printCommand commandList
    putStrLn ""
    where maxCommandLen = 4 + maximum [ length c | (c,_,_,_) <- commandList ]
          spaces = repeat ' '
          printCommand (com, hlp, _, _) = do
              putStrLn $ ' ' : com 
                      ++ take (maxCommandLen - length com) spaces 
                      ++ hlp

helpCommand (x:_) = case find (\(x',_,_,_) -> x' == x) commandList of
     Just (_, hlp, _, options) -> do
         printVer
         putStrLn $ usageInfo hlp options
     Nothing -> putStrLn $ "Unknown command " ++ x

commandList :: [(String, String, [String] -> IO (), [OptDescr (Flag, String)])]
commandList = 
    [ ("cleanup", "Perform trivial simplification on formula", transformParseFormula reduce, commonOption)
    , ("eval", "Try to evaluate/reduce the formula", transformParseFormula reduce, commonOption)
    , ("format", "Load and display the formula in ASCII Art", formatCommand, commonOption)
    , ("help", "Ask specific help for a command, or this", helpCommand, [])
    ]

reducedCommand :: [(String, [String] -> IO ())]
reducedCommand = map (\(n,_,a,_) -> (n,a)) commandList

main :: IO ()
main = do
    args <- getArgs
    if null args
       then error "No command give, try the help command"
       else case lookup (args !! 0) reducedCommand of
                 Just c -> c $ tail args
                 Nothing -> error $ "Unknown command " ++ (args !! 0)
              
