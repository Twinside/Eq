import System.Environment
import EqManips.Types
import EqManips.Linker
import EqManips.Renderer.Ascii
import Text.ParserCombinators.Parsec.Prim( runParser )

import System.Exit
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

formatCommand :: [String] -> IO Bool
formatCommand args = do
    formulaText <- input
    let formula = runParser expr () "FromFile" formulaText
    output <- outputFile
    either (\err -> do print "Error : "
                       print err
                       hClose output
                       return False)
           (\formula' -> do 
                hPutStrLn output. formatFormula $ linkFormula formula'
                hClose output
                return True)
           formula
     where (opt, left, _) = getOpt Permute formatOption args
           (input, outputFile) = getInputOutput opt left

printErrors :: [(Formula, String)] -> IO ()
printErrors =
    mapM_ (\(f,s) -> do putStrLn s
                        putStrLn $ formatFormula f) 

transformParseFormula :: (Formula -> EqContext Formula) -> [String]
                      -> IO Bool
transformParseFormula operation args = do
    formulaText <- input
    finalFile <- outputFile
    let formula = runParser expr () "FromFile" formulaText
    either (\err -> do print "Error : " 
                       print err
                       return False)
           (\formula' -> do
               let rez = performTransformation 
                                . operation 
                                $ linkFormula formula'
               printErrors $ errorList rez
               hPutStr finalFile . formatFormula $ result rez

#ifdef _DEBUG
               hPutStrLn finalFile "\n####### <TRACE> #########"
               printTrace finalFile rez
               hPutStrLn finalFile "####### </TRACE> #########"
#endif

               return . null $ errorList rez)
           formula

     where (opt, left, _) = getOpt Permute formatOption args
           (input, outputFile) = getInputOutput opt left

printVer :: IO ()
printVer = 
    putStrLn $ "EqManips " ++ version ++ " command list"

helpCommand :: [String] -> IO Bool
helpCommand [] = do
    printVer
    putStrLn ""
    mapM_ printCommand commandList
    putStrLn ""
    return True
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
         return True
     Nothing -> do putStrLn $ "Unknown command " ++ x
                   return False

commandList :: [(String, String, [String] -> IO Bool, [OptDescr (Flag, String)])]
commandList = 
    [ ("cleanup", "Perform trivial simplification on formula", transformParseFormula reduce, commonOption)
    , ("eval", "Try to evaluate/reduce the formula", transformParseFormula reduce, commonOption)
    , ("format", "Load and display the formula in ASCII Art", formatCommand, commonOption)
    , ("help", "Ask specific help for a command, or this", helpCommand, [])
    ]

reducedCommand :: [(String, [String] -> IO Bool)]
reducedCommand = map (\(n,_,a,_) -> (n,a)) commandList

main :: IO ()
main = do
    args <- getArgs
    if null args
       then error "No command give, try the help command"
       else case lookup (args !! 0) reducedCommand of
                 Just c -> (c $ tail args) >>= systemReturn
                 Nothing -> error $ "Unknown command " ++ (args !! 0)
     where systemReturn True = exitWith ExitSuccess
           systemReturn False = exitWith $ ExitFailure 1
              
