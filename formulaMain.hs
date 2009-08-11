import System.Environment
import EqManips.Types
import EqManips.Algorithm.Utils
import EqManips.Renderer.Ascii

import System.Exit
import System.IO
import System.Console.GetOpt

import Data.List( find )

-- Just to be able to compile...
import EqManips.Algorithm.Eval
import EqManips.EvaluationContext
import Preprocessor

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

preprocOptions :: [OptDescr (Flag, String)]
preprocOptions = commonOption

formatOption :: [OptDescr (Flag, String)]
formatOption = commonOption

-- | Helper function to get file names for input/output
getInputOutput :: [(Flag, String)] -> [String] -> (IO String, IO Handle)
getInputOutput opts args = (inputFile, outputFile)
   where outputFile = maybe (return stdout) (\name -> openFile name WriteMode)
                            (lookup Output opts)
         inputFile = maybe (return $ args !! 0) readFile
                           (lookup Input opts)

-- | Command which just format an equation
-- without affecting it's form.
formatCommand :: [String] -> IO Bool
formatCommand args = do
    formulaText <- input
    let formula = parseFormula formulaText
    output <- outputFile
    either (\err -> do print "Error : "
                       print err
                       hClose output
                       return False)
           (\formula' -> do 
                hPutStrLn output $ formatFormula formula'
                hClose output
                return True)
           formula
     where (opt, left, _) = getOpt Permute formatOption args
           (input, outputFile) = getInputOutput opt left

printErrors :: [(Formula, String)] -> IO ()
printErrors =
    mapM_ (\(f,s) -> do putStrLn s
                        putStrLn $ formatFormula f) 

preprocessCommand :: [String] -> IO Bool
preprocessCommand args =
    if inName == ""
       then do print "Error, no input name given"
               return False
       else do
           outFile <- processFile inName
           writeFile outName outFile
           return True
     where (opts, _, _) = getOpt Permute preprocOptions args
           inName = maybe "" id (lookup Input opts)
           outName = maybe inName id (lookup Output opts)

transformParseFormula :: (Formula -> EqContext Formula) -> [String]
                      -> IO Bool
transformParseFormula operation args = do
    formulaText <- input
    finalFile <- outputFile
    let formulaList = parseProgramm formulaText
    either (\err -> print "Error : " >> print err >> return False)
           (\formulal -> do
               let rez = performLastTransformation $
                                mapM operation formulal
#ifdef _DEBUG
               hPutStrLn finalFile "\n####### <TRACE> #########"
               printTrace finalFile rez
               hPutStrLn finalFile "####### </TRACE> #########"
#endif

               print $ result rez
               printErrors $ errorList rez
               hPutStr finalFile . formatFormula $ result rez

               return . null $ errorList rez)
           formulaList

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
    , ("preprocess", "Parse a source file and apply inline action in it", preprocessCommand, commonOption)
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
              
