import EqManips.Types
import EqManips.Algorithm.Utils
import EqManips.Algorithm.Cleanup
import EqManips.Renderer.Ascii
import EqManips.Renderer.Latex
import EqManips.Renderer.Mathml
import EqManips.Renderer.RenderConf

import EqManips.Renderer.Ascii2DGrapher

import CharArray

#ifdef _DEBUG
import EqManips.Renderer.Sexpr
#endif

import Control.Monad

import System.Environment
import System.Exit
import System.IO
import qualified System.IO as Io

import System.Console.GetOpt

import Data.List( find, intersperse, foldl' )
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

import Repl

-- Debugging
{-import EqManips.Renderer.CharRender-}

data Flag =
      Output
    | Input
    | Unicode
    | SupportedFunction
    | SupportedOperators
    | SupportedPreprocLanguages

    -- for plotting
    | PlotWidth
    | PlotHeight
    | XBeg
    | XEnd
    | YBeg
    | YEnd
    | XLogScale
    | YLogScale
    | DrawXaxis
    | DrawYaxis
    | Draw0axis

    | NoDrawXLabel
    | NoDrawYLabel

    | XLabelPrecision
    | YLabelPrecision

    | XLabelSpacing
    | YLabelSpacing
    deriving (Eq, Show)

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

plotOption :: [OptDescr (Flag, String)]
plotOption =
    [ Option "x" ["xBegin"] (ReqArg ((,) XBeg) "XBEG") "Beginning of plot (x), float"
    , Option ""  ["xe", "xEnd"] (ReqArg ((,) XEnd) "XEND") "End of plot (x), float"
    , Option "y" ["yBegin"] (ReqArg ((,) YBeg) "YBEG") "Beginning of plot (y), float"
    , Option ""  ["ye", "yEnd"] (ReqArg ((,) YEnd) "YEnd") "End of plot (y), float"
    , Option "w" ["width"]  (ReqArg ((,) PlotWidth) "Width") "Plotting width, int"
    , Option "h" ["height"] (ReqArg ((,) PlotHeight) "height") "Plotting height, int"
    , Option "" ["lx", "logwidth"] (NoArg (XLogScale,""))
                  "Plot with a logrithmic scale in x"
    , Option "" ["ly", "logheight"] (NoArg (YLogScale,""))
                  "Plot with a logrithmic scale in y"
    , Option "" ["ax", "xaxis"] (NoArg (DrawXaxis,""))
                  "Draw the X axis on the graph"
    , Option "" ["ay", "yaxis"] (NoArg (DrawYaxis,""))
                  "Draw the Y axis on the graph"
    , Option "" ["a0", "zeroaxis"] (NoArg (Draw0axis,""))
                  "Draw the 0 axis on the graph"
    , Option "" ["nlx", "nolabelx"] (NoArg (NoDrawXLabel,""))
                  "Don't draw label on x Axis"
    , Option "" ["nly", "nolabely"] (NoArg (NoDrawYLabel,""))
                  "Don't draw label on Y Axis"
    , Option "" ["lpx", "xlabelprecision"] 
                (ReqArg ((,) XLabelPrecision) "p") 
                "Display label on x axis with 'p' decimals"
    , Option "" ["lpy", "ylabelprecision"] 
                (ReqArg ((,) YLabelPrecision) "p") 
                "Display label on y axis with 'p' decimals"
    , Option "" ["spx", "labelspacingx"]
                (ReqArg ((,) XLabelSpacing) "s")
                "Put a label evry 's' chars on x axis"
    , Option "" ["spy", "labelspacingy"]
                (ReqArg ((,) YLabelSpacing) "s")
                "Put a label evry 's' chars on y axis"
    ]

preparePlotConf :: PlotConf -> (Flag, String) -> PlotConf
preparePlotConf conf (PlotWidth, val) = 
    conf { xDim = (xDim conf){ projectionSize = read val } }
preparePlotConf conf (PlotHeight, val) =
    conf { yDim = (yDim conf){ projectionSize = read val }}
preparePlotConf conf (XBeg, val) =
    conf { xDim = (xDim conf){ minVal = read val }}
preparePlotConf conf (XEnd, val) =
    conf { xDim = (xDim conf){ maxVal = read val }}
preparePlotConf conf (YBeg, val) =
    conf { yDim = (yDim conf){ minVal = read val }}
preparePlotConf conf (YEnd, val) =
    conf { yDim = (yDim conf){ maxVal = read val }}
preparePlotConf conf (XLogScale, _) =
    conf { xDim = (xDim conf){ scaling = Logarithmic } }
preparePlotConf conf (YLogScale, _) =
    conf { yDim = (yDim conf){ scaling = Logarithmic } }
preparePlotConf conf (DrawXaxis, _) =
    conf { xDim = (xDim conf){ drawAxis = True } }
preparePlotConf conf (DrawYaxis, _) =
    conf { yDim = (yDim conf){ drawAxis = True } }
preparePlotConf conf (Draw0axis, _) =
    conf { draw0Axis = True }
preparePlotConf conf (NoDrawXLabel, _) =
    conf { xDim = (xDim conf){ labelEvery = Nothing } }
preparePlotConf conf (NoDrawYLabel, _) =
    conf { yDim = (yDim conf){ labelEvery = Nothing } }
preparePlotConf conf (XLabelSpacing, val) =
    conf { xDim = (xDim conf){ labelEvery = Just $ read val} }
preparePlotConf conf (YLabelSpacing, val) =
    conf { yDim = (yDim conf){ labelEvery = Just $ read val} }
preparePlotConf conf (XLabelPrecision, val) =
    conf { xDim = (xDim conf){ labelPrecision = read val} }
preparePlotConf conf (YLabelPrecision, val) =
    conf { xDim = (yDim conf){ labelPrecision = read val} }
preparePlotConf conf _ = conf

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

         infiler "-" = Io.hGetContents stdin
         infiler f = Io.readFile f

filterCommand :: (String -> String) -> [String] -> IO Bool
filterCommand transformator args = do
    text <- input
    output <- outputFile
    Io.putStr text
    Io.putStr "==========================================\n"
    Io.hPutStrLn output $ transformator text
    Io.putStr "==========================================\n\n"
    hClose output
    return True
     where (opt, rest, _) = getOpt Permute formatOption args
           (input, outputFile) = getInputOutput opt rest

-- | Command which just format an equation
-- without affecting it's form.
formatCommand :: (Conf -> Formula TreeForm -> String) -> [String] -> IO Bool
formatCommand formulaFormater args = do
    formulaText <- input
    let formula = perfectParse formulaText
    output <- outputFile
    either (parseErrorPrint output)
           (\formula' -> do 
                Io.hPutStrLn output . formulaFormater conf $ treeIfyFormula formula'
                hClose output
                return True)
           formula
     where (opt, rest, _) = getOpt Permute formatOption args
           (input, outputFile) = getInputOutput opt rest
           conf = defaultRenderConf{ useUnicode = Unicode `lookup` opt /= Nothing }

printErrors :: [(Formula TreeForm, String)] -> IO ()
printErrors =
    mapM_ (\(f,s) -> do Io.putStrLn s
                        Io.putStrLn $ formatFormula defaultRenderConf f) 

parseErrorPrint :: (Show a) => Handle -> a -> IO Bool
parseErrorPrint finalFile err = do
    Io.hPutStr finalFile "Error : "
    Io.hPutStr finalFile $ show err
    hClose finalFile
    return False

-- | Give the user some information about the defined
-- elements. This help cannot lie =)
introspect :: [String] -> IO Bool
introspect args = do
    when ((SupportedFunction, "") `elem` opts)
         (do Io.putStrLn "Supported functions :"
             Io.putStrLn "====================="
             Io.putStrLn "Built-in functions :"
             Io.putStrLn "--------------------"
             mapM_ (Io.putStrLn . ('\t':) . fst) $ unaryFunctions ++ metaFunctionList 
             mapM_ Io.putStrLn
                    [ '\t': name ++ '(' : (concat . intersperse ", " $ map fst params) ++ ")"
                                | (name, (_,_,params,_)) <- multiParamsFunctions]

             Io.putStrLn "\nBase library functions :"
             Io.putStrLn "------------------------"
             mapM_ (Io.putStrLn . ('\t':)) $ Map.keys defaultSymbolTable 
             )

    when ((SupportedOperators, "") `elem` opts)
         (do Io.putStrLn "Supported operators :   "
             Io.putStrLn "====================="

             Io.putStrLn "\nBinary operators (Priority - name - description)"
             Io.putStrLn "------------------------------------------------"
             let names = [n | (_,(_,n,_)) <- binopDefs]
                 maxName = maximum $ map length names
                 binFormat (prio, name, descr) = '\t':
                     show prio ++ " - " ++ name
                               ++ replicate (maxName - length name) ' '
                               ++ " - " ++ descr
             mapM_ (Io.putStrLn . binFormat . snd) binopDefs

             Io.putStrLn "\nUnary operators (name - description)"
             Io.putStrLn "------------------------------------"
             mapM_ (Io.putStrLn . (\(_, n, d) -> '\t' : n ++ " - " ++ d)) realUnopOperators)

    when ((SupportedPreprocLanguages, "") `elem` opts)
         (do Io.putStrLn "Supported languages for preprocessing :"
             Io.putStrLn "======================================="
             let maxi = maximum [ length n | (n, _) <- kindAssociation ]
                 preprocFormat (ext, lang) =
                     '\t' : ext ++ replicate (maxi - length ext) ' '
                                ++ " - "
                                ++ languageName lang
             mapM_ (Io.putStrLn . preprocFormat) kindAssociation 
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
           Io.writeFile outName outFile
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
               mapM_ (\a-> do Io.hPutStr finalFile $ sexprRender a
                              Io.hPutStr finalFile "\n") formulal
               hFlush finalFile
#endif
               let rez = performLastTransformationWithContext defaultSymbolTable
                       $ mapM operation formulal
#ifdef _DEBUG
               Io.hPutStrLn finalFile "\n####### <TRACE> #########"
               printTrace finalFile rez
               Io.hPutStrLn finalFile "####### </TRACE> #########\n"
               Io.hPutStrLn finalFile . show $ result rez
               Io.hPutStrLn finalFile . sexprRender $ result rez
#endif
               printErrors $ errorList rez
               Io.hPutStr finalFile . formatFormula conf . treeIfyFormula $ result rez
               hClose finalFile

               return . null $ errorList rez)
           formulaList

     where (opt, rest, _) = getOpt Permute formatOption args
           (input, outputFile) = getInputOutput opt rest
           conf = defaultRenderConf{ useUnicode = Unicode `lookup` opt /= Nothing }

plotCommand :: [String] -> IO Bool
plotCommand args = do
    formulaText <- input
    finalFile <- outputFile

    let formulaList = parseProgramm formulaText
    either (parseErrorPrint finalFile)
           (\formulal -> do
               case plot2DExpression plotConf . unTagFormula $ head formulal of
                Left err -> do
                    Io.hPutStr finalFile err
                    hClose finalFile
                    return False

                Right v -> do
                    Io.hPutStr finalFile $ charArrayToString  v
                    return True)
           formulaList
     where (opt, rest, _) = getOpt Permute (commonOption ++ plotOption) args
           plotConf = foldl' preparePlotConf defaultPlotConf 
                             opt
           (input, outputFile) = getInputOutput opt rest

printVer :: IO ()
printVer = 
    Io.putStrLn $ "EqManips " ++ version ++ " command list"

helpCommand :: [String] -> IO Bool
helpCommand [] = do
    printVer
    Io.putStrLn ""
    mapM_ printCommand commandList
    Io.putStrLn ""
    return True
    where maxCommandLen = 4 + maximum [ length c | (c,_,_,_) <- commandList ]
          spaces = repeat ' '
          printCommand (com, hlp, _, _) =
              Io.putStrLn $ ' ' : com 
                           ++ take (maxCommandLen - length com) spaces 
                           ++ hlp

helpCommand (x:_) = case find (\(x',_,_,_) -> x' == x) commandList of
     Just (_, hlp, _, options) -> do
         printVer
         Io.putStrLn $ usageInfo hlp options
         return True
     Nothing -> do Io.putStrLn $ "Unknown command " ++ x
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
               Io.hPutStrLn stdout "\n####### <TRACE> #########"
               printTrace stdout rez
               Io.hPutStrLn stdout "####### </TRACE> #########\n"
               Io.hPutStrLn stdout . sexprRender $ result rez
#endif
               printErrors $ errorList rez
               Io.hPutStr stdout . formatFormula . treeIfyFormula $ result rez
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
    , ("interactive", "Invoke Eq as an interactive prompt",
                (\_ -> do repl evalGlobalLossyStatement
                          return True), [])
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
    , ("plot", "Print an ASCII-art plot of the given function"
            , plotCommand, commonOption ++ plotOption)
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
              
