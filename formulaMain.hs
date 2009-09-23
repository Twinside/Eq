import System.Environment
import EqManips.Types
import EqManips.Algorithm.Utils
import EqManips.Algorithm.Cleanup
import EqManips.Renderer.Ascii
import EqManips.Renderer.Latex
import EqManips.Renderer.Mathml
import System.Exit
import System.IO
import System.Console.GetOpt

import Data.List( find )

-- Just to be able to compile...
import EqManips.Algorithm.Eval
import EqManips.EvaluationContext
import Preprocessor

-- BLEH
import CharArray 

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
formatCommand :: (Formula -> String) -> [String] -> IO Bool
formatCommand formater args = do
    formulaText <- input
    let formula = parseFormula formulaText
    output <- outputFile
    either (parseErrorPrint output)
           (\formula' -> do 
                hPutStrLn output $ formater formula'
                hClose output
                return True)
           formula
     where (opt, left, _) = getOpt Permute formatOption args
           (input, outputFile) = getInputOutput opt left

printErrors :: [(Formula, String)] -> IO ()
printErrors =
    mapM_ (\(f,s) -> do putStrLn s
                        putStrLn $ formatFormula f) 

parseErrorPrint :: (Show a) => Handle -> a -> IO Bool
parseErrorPrint finalFile err = do
    hPutStr finalFile "Error : "
    hPutStr finalFile $ show err
    hClose finalFile
    return False

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
    either (parseErrorPrint finalFile)
           (\formulal -> do
               let rez = performLastTransformation $
                                mapM operation formulal
               mapM (\a-> do hPutStr finalFile $ show a
                             hPutStr finalFile "\n\n") formulal
#ifdef _DEBUG
               hPutStrLn finalFile "\n####### <TRACE> #########"
               printTrace finalFile rez
               hPutStrLn finalFile "####### </TRACE> #########\n"
#endif
               let (array, _size) = renderFormula $ result rez
                   text = unlines $ linesOfArray array
               hPrint finalFile $ result rez
               hPutStrLn finalFile ""
               printErrors $ errorList rez
               hPutStr finalFile text
               {-hPutStr finalFile . formatFormula $ result rez-}
               hClose finalFile

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
    [ ("cleanup", "Perform trivial simplification on formula"
            , transformParseFormula (return . cleanup), commonOption)
    , ("eval", "Try to evaluate/reduce the formula"
            , transformParseFormula evalGlobalStatement, commonOption)
    , ("format", "Load and display the formula in ASCII Art"
            , formatCommand formatFormula, commonOption)
    , ("latexify", "Translate the formula into latex"
            , formatCommand latexRender, commonOption)
    , ("mathmlify", "Translate the formula into MathML"
            , formatCommand mathmlRender, commonOption)
    , ("toraw", "Show internal representation of formula"
            , formatCommand show, commonOption)
    , ("help", "Ask specific help for a command, or this"
            , helpCommand, [])
    , ("preprocess", "Parse a source file and apply inline action in it"
            , preprocessCommand, commonOption)
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
              
