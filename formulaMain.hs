import EqManips.Types
import EqManips.Algorithm.Utils
import EqManips.Algorithm.Cleanup
import EqManips.Renderer.Ascii
import EqManips.Renderer.Latex
import EqManips.Renderer.Mathml
import EqManips.Renderer.RenderConf
#ifdef _DEBUG
import EqManips.Renderer.Sexpr
#endif

import Control.Monad

import System.Environment
import System.Exit
import System.IO
--import qualified System.IO.UTF8 as Utf8
import qualified System.IO as Utf8

import System.Console.GetOpt

import Data.List( find, intersperse )
import Data.Maybe( fromMaybe )

import qualified Data.Map as Map

-- Just to be able to compile...
import EqManips.Algorithm.Eval
import EqManips.EvaluationContext
import EqManips.Preprocessor
import EqManips.Linker
import EqManips.BaseLibrary
import EqManips.InputParser.MathML
import EqManips.InputParser.EqCode

-- Debugging
{-import EqManips.Renderer.CharRender-}

data Flag =
      Output
    | Input
    | Unicode
    | SupportedFunction
    | SupportedOperators
    | SupportedPreprocLanguages
    deriving Eq

version :: String
version = "0.2"

commonOption :: [OptDescr (Flag, String)]
commonOption =
    [ Option "o"  ["output"] (ReqArg ((,) Output) "FILE") "output FILE"
    , Option "f"  ["file"] (ReqArg ((,) Input) "FILE") "input FILE, use - for stdin"
    , Option "u"  ["unicode"] (NoArg (Unicode, "")) "Output with unicode character set"
    ]

askingOption :: [OptDescr (Flag, String)]
askingOption =
    [ Option "" ["functions"] (NoArg (SupportedFunction,""))
                "Ask for defined function list"
    , Option "" ["operators"] (NoArg (SupportedOperators,""))
                "Ask for defined operator list"
    , Option "" ["languages"] (NoArg (SupportedPreprocLanguages,""))
                "Ask for supported languages for the preprocessor"
    ]

preprocOptions :: [OptDescr (Flag, String)]
preprocOptions = commonOption

formatOption :: [OptDescr (Flag, String)]
formatOption = commonOption

-- | Helper function to get file names for input/output
getInputOutput :: [(Flag, String)] -> [String] -> (IO String, IO Handle)
getInputOutput opts args = ( inputFile
                           , do o <- outputFile 
                                hSetEncoding o utf8
                                return o)
   where outputFile = maybe (return stdout) (flip openFile WriteMode)
                            (lookup Output opts)

         inputFile = maybe (return $ head args) infiler
                           (lookup Input opts)

         infiler "-" = Utf8.hGetContents stdin
         infiler f = Utf8.readFile f

filterCommand :: (String -> String) -> [String] -> IO Bool
filterCommand transformator args = do
    text <- input
    output <- outputFile
    Utf8.putStr text
    Utf8.putStr "==========================================\n"
    Utf8.hPutStrLn output $ transformator text
    Utf8.putStr "==========================================\n\n"
    hClose output
    return True
     where (opt, left, _) = getOpt Permute formatOption args
           (input, outputFile) = getInputOutput opt left

-- | Command which just format an equation
-- without affecting it's form.
formatCommand :: (Conf -> Formula TreeForm -> String) -> [String] -> IO Bool
formatCommand formulaFormater args = do
    formulaText <- input
    let formula = perfectParse formulaText
    output <- outputFile
    either (parseErrorPrint output)
           (\formula' -> do 
                Utf8.hPutStrLn output . formulaFormater conf $ treeIfyFormula formula'
                hClose output
                return True)
           formula
     where (opt, left, _) = getOpt Permute formatOption args
           (input, outputFile) = getInputOutput opt left
           conf = defaultRenderConf{ useUnicode = Unicode `lookup` opt /= Nothing }

printErrors :: [(Formula TreeForm, String)] -> IO ()
printErrors =
    mapM_ (\(f,s) -> do Utf8.putStrLn s
                        Utf8.putStrLn $ formatFormula defaultRenderConf f) 

parseErrorPrint :: (Show a) => Handle -> a -> IO Bool
parseErrorPrint finalFile err = do
    Utf8.hPutStr finalFile "Error : "
    Utf8.hPutStr finalFile $ show err
    hClose finalFile
    return False

-- | Give the user some information about the defined
-- elements. This help cannot lie =)
introspect :: [String] -> IO Bool
introspect args = do
    when ((SupportedFunction, "") `elem` opts)
         (do Utf8.putStrLn "Supported functions :"
             Utf8.putStrLn "====================="
             Utf8.putStrLn "Built-in functions :"
             Utf8.putStrLn "--------------------"
             mapM_ (Utf8.putStrLn . ('\t':) . fst) $ unaryFunctions ++ metaFunctionList 
             mapM_ Utf8.putStrLn
                    [ '\t': name ++ '(' : (concat . intersperse ", " $ map fst params) ++ ")"
                                | (name, (_,_,params,_)) <- multiParamsFunctions]

             Utf8.putStrLn "\nBase library functions :"
             Utf8.putStrLn "------------------------"
             mapM_ (Utf8.putStrLn . ('\t':)) $ Map.keys defaultSymbolTable 
             )

    when ((SupportedOperators, "") `elem` opts)
         (do Utf8.putStrLn "Supported operators :   "
             Utf8.putStrLn "====================="

             Utf8.putStrLn "\nBinary operators (Priority - name - description)"
             Utf8.putStrLn "------------------------------------------------"
             let names = [n | (_,(_,n,_)) <- binopDefs]
                 maxName = maximum $ map length names
                 binFormat (prio, name, descr) = '\t':
                     show prio ++ " - " ++ name
                               ++ replicate (maxName - length name) ' '
                               ++ " - " ++ descr
             mapM_ (Utf8.putStrLn . binFormat . snd) binopDefs

             Utf8.putStrLn "\nUnary operators (name - description)"
             Utf8.putStrLn "------------------------------------"
             mapM_ (Utf8.putStrLn . (\(_, n, d) -> '\t' : n ++ " - " ++ d)) realUnopOperators)

    when ((SupportedPreprocLanguages, "") `elem` opts)
         (do Utf8.putStrLn "Supported languages for preprocessing :"
             Utf8.putStrLn "======================================="
             let maxi = maximum [ length n | (n, _) <- kindAssociation ]
                 preprocFormat (ext, lang) =
                     '\t' : ext ++ replicate (maxi - length ext) ' '
                                ++ " - "
                                ++ languageName lang
             mapM_ (Utf8.putStrLn . preprocFormat) kindAssociation 
             )

    return True
   where (opts, _, _) = getOpt Permute askingOption args

preprocessCommand :: [String] -> IO Bool
preprocessCommand args =
    if inName == ""
       then do print "Error, no input name given"
               return False
       else do
           outFile <- processFile inName
           Utf8.writeFile outName outFile
           return True
     where (opts, _, _) = getOpt Permute preprocOptions args
           inName = fromMaybe "" (lookup Input opts)
           outName = fromMaybe inName (lookup Output opts)

transformParseFormula :: (Formula ListForm -> EqContext (Formula ListForm)) -> [String]
                      -> IO Bool
transformParseFormula operation args = do
    formulaText <- input
    finalFile <- outputFile

    let formulaList = parseProgramm formulaText
    either (parseErrorPrint finalFile)
           (\formulal -> do
#ifdef _DEBUG
               mapM_ (\a-> do Utf8.hPutStr finalFile $ sexprRender a
                              Utf8.hPutStr finalFile "\n") formulal
               hFlush finalFile
#endif
               let rez = performLastTransformationWithContext defaultSymbolTable
                       $ mapM operation formulal
#ifdef _DEBUG
               Utf8.hPutStrLn finalFile "\n####### <TRACE> #########"
               printTrace finalFile rez
               Utf8.hPutStrLn finalFile "####### </TRACE> #########\n"
               Utf8.hPutStrLn finalFile . show $ result rez
               Utf8.hPutStrLn finalFile . sexprRender $ result rez
#endif
               printErrors $ errorList rez
               Utf8.hPutStr finalFile . formatFormula conf . treeIfyFormula $ result rez
               hClose finalFile

               return . null $ errorList rez)
           formulaList

     where (opt, left, _) = getOpt Permute formatOption args
           (input, outputFile) = getInputOutput opt left
           conf = defaultRenderConf{ useUnicode = Unicode `lookup` opt /= Nothing }

printVer :: IO ()
printVer = 
    Utf8.putStrLn $ "EqManips " ++ version ++ " command list"

helpCommand :: [String] -> IO Bool
helpCommand [] = do
    printVer
    Utf8.putStrLn ""
    mapM_ printCommand commandList
    Utf8.putStrLn ""
    return True
    where maxCommandLen = 4 + maximum [ length c | (c,_,_,_) <- commandList ]
          spaces = repeat ' '
          printCommand (com, hlp, _, _) =
              Utf8.putStrLn $ ' ' : com 
                           ++ take (maxCommandLen - length com) spaces 
                           ++ hlp

helpCommand (x:_) = case find (\(x',_,_,_) -> x' == x) commandList of
     Just (_, hlp, _, options) -> do
         printVer
         Utf8.putStrLn $ usageInfo hlp options
         return True
     Nothing -> do Utf8.putStrLn $ "Unknown command " ++ x
                   return False

#ifdef _GHCI_DEBUG
transformParseDebug :: (Formula ListForm -> EqContext (Formula ListForm)) -> String
                    -> IO Bool
transformParseDebug operation formulaText = do
    let formulaList = parseProgramm formulaText
    either (parseErrorPrint stdout)
           (\formulal -> do
               let rez = performLastTransformationWithContext defaultSymbolTable
                       $ mapM operation formulal
#ifdef _DEBUG
               mapM (\a-> do hPutStr stdout $ sexprRender a
                             hPutStr stdout "\n") formulal
               Utf8.hPutStrLn stdout "\n####### <TRACE> #########"
               printTrace stdout rez
               Utf8.hPutStrLn stdout "####### </TRACE> #########\n"
               Utf8.hPutStrLn stdout . sexprRender $ result rez
#endif
               printErrors $ errorList rez
               Utf8.hPutStr stdout . formatFormula . treeIfyFormula $ result rez
               return True
               )
           formulaList

evalDebug :: String -> IO Bool
evalDebug = transformParseDebug evalGlobalLossyStatement
#endif

commandList :: [(String, String, [String] -> IO Bool, [OptDescr (Flag, String)])]
commandList = 
    [ ("cleanup", "Perform trivial simplification on formula"
            , transformParseFormula (return . cleanup), commonOption)
    , ("eval", "Try to evaluate/reduce the formula"
            , transformParseFormula evalGlobalLossyStatement, commonOption)
    , ("exacteval", "Try to evaluate/reduce the formula, without performing lossy operation"
            , transformParseFormula evalGlobalLosslessStatement, commonOption)
    , ("format", "Load and display the formula in ASCII Art"
            , formatCommand formatFormula, commonOption)
    , ("latexify", "Translate the formula into latex"
            , formatCommand latexRender, commonOption)
    , ("mathmlify", "Translate the formula into MathML"
            , formatCommand mathmlRender, commonOption)
    , ("toraw", "Show internal representation of formula"
            , formatCommand $ const show, commonOption)
    , ("help", "Ask specific help for a command, or this"
            , helpCommand, [])
    , ("preprocess", "Parse a source file and apply inline action in it"
            , preprocessCommand, commonOption)
    , ("demathmlify", "Try to transform a MathML Input to EQ language"
            , filterCommand mathMlToEqLang', commonOption)
    , ("show"       , "Try to retrieve some information about supported options"
            , introspect, askingOption)
    ]

reducedCommand :: [(String, [String] -> IO Bool)]
reducedCommand = map (\(n,_,a,_) -> (n,a)) commandList

main :: IO ()
main = do
    args <- getArgs
    if null args
       then error "No command given, try the help command"
       else case lookup (head args) reducedCommand of
                 Just c -> c (tail args) >>= systemReturn
                 Nothing -> error $ "Unknown command " ++ head args
     where systemReturn True = exitWith ExitSuccess
           systemReturn False = exitWith $ ExitFailure 1
              
