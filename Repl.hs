module Repl( repl ) where

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

repl :: Evaluator -> IO ()
repl evaluator = do
    putStrLn "Eq - interactive mode"
    putStrLn "exit to quit the program\n"
    doer (Just defaultSymbolTable)

  where doer (Just c) = evalExpr evaluator c >>= doer
        doer Nothing = return ()

printErrors :: [(Formula TreeForm, String)] -> IO ()
printErrors =
    mapM_ (\(f,s) -> do putStrLn s
                        putStrLn $ formatFormula defaultRenderConf f) 

parseErrorPrint :: (Show a) => b -> a -> IO b
parseErrorPrint c err = do
    putStr "Error : "
    putStr $ show err
    return c

evalExpr :: Evaluator -> Context -> IO (Maybe Context)
evalExpr operation prevContext = do
    putStr "> "
    hFlush stdout
    exprText <- getLine
    case exprText of
         []     -> evalExpr operation prevContext
         "exit" -> return Nothing
         _      -> do
            let formulaList = parseProgramm exprText
            either (parseErrorPrint (Just prevContext))
                   (\formulal -> do
                       let rez = performLastTransformationWithContext prevContext
                               $ mapM operation formulal

                       printErrors $ errorList rez
                       putStr . formatFormula defaultRenderConf
                              . treeIfyFormula $ result rez
                       return . Just $ context rez
                       )
                   formulaList

