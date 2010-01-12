module Repl( repl ) where

import Control.Monad

import qualified Data.Map as Map

import EqManips.Algorithm.Utils
import EqManips.Types
import EqManips.Renderer.Ascii
import EqManips.BaseLibrary
import EqManips.InputParser.EqCode
import EqManips.EvaluationContext

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
                        putStrLn $ formatFormula f) 

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
    if exprText == "exit"
       then return Nothing
       else do
        let formulaList = parseProgramm exprText
        either (parseErrorPrint (Just prevContext))
               (\formulal -> do
                   let rez = performLastTransformationWithContext prevContext
                           $ mapM operation formulal

                   printErrors $ errorList rez
                   putStr . formatFormula . treeIfyFormula $ result rez
                   return . Just $ context rez
                   )
               formulaList

