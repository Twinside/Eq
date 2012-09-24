module Language.Eq.Repl( repl ) where

import qualified Data.Map as Map

import Language.Eq.Algorithm.Utils
import Language.Eq.Types
import Language.Eq.Renderer.Ascii
import Language.Eq.Renderer.RenderConf
import Language.Eq.BaseLibrary
import Language.Eq.InputParser.EqCode
import Language.Eq.EvaluationContext

import System.IO

type Context = Map.Map String (Formula ListForm)
type Evaluator = Formula ListForm -> EqContext (Formula ListForm)


data ReplInfo =
      ValidContext !Int !Context
    | EndOfRepl

repl :: Evaluator -> IO ()
repl evaluator = do
    putStrLn "Eq - interactive mode"
    putStrLn "exit to quit the program\n"
    doer (ValidContext 1 $ defaultSymbolTable `Map.union` initialReplContextInfo)

  where doer c@(ValidContext _ _) = evalExpr evaluator c >>= doer
        doer EndOfRepl = return ()

printErrors :: [(Formula TreeForm, String)] -> IO ()
printErrors =
    mapM_ (\(f,s) -> do putStrLn s
                        putStrLn $ formatFormula defaultRenderConf f) 

parseErrorPrint :: (Show a) => b -> a -> IO b
parseErrorPrint c err = do
    putStr "Error : "
    putStr $ show err
    return c

queryVarName, answerVarName :: String
queryVarName = "query"
answerVarName = "answers"

initialReplContextInfo :: Context
initialReplContextInfo = Map.fromList 
    [ (answerVarName, Formula $ list []), (queryVarName, Formula $ list [])]

addToList :: Formula ListForm -> Formula ListForm -> Formula ListForm
addToList (Formula toAdd) (Formula (List _ lst)) = Formula . list $ lst ++ [toAdd]
addToList _   f = f

evalExpr :: Evaluator -> ReplInfo -> IO ReplInfo
evalExpr operation ctxt@(ValidContext askId prevContext) = do
    putStr $ '[' : show askId ++ "] > "
    hFlush stdout
    exprText <- getLine
    case exprText of
         []     -> evalExpr operation ctxt
         "exit" -> return EndOfRepl
         _      -> do
            let formulaList = parseProgramm exprText
            either (parseErrorPrint ctxt)
                   (\formulal -> do
                       let rez = performLastTransformationWithContext prevContext
                               $ mapM operation formulal

                       printErrors $ errorList rez
                       putStr . formatFormula defaultRenderConf
                              . treeIfyFormula $ result rez
                       let transformedContext = context rez
                           answers = transformedContext Map.! answerVarName
                           queries = transformedContext Map.! queryVarName
                           newInfo = Map.fromList 
                                [(answerVarName, result rez `addToList` answers)
                                ,(queryVarName, last formulal `addToList` queries)]
                       return . ValidContext (askId + 1) 
                              $ newInfo `Map.union` transformedContext
                       )
                   formulaList
evalExpr _ EndOfRepl = return EndOfRepl

