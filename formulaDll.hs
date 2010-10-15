{-# LANGUAGE ForeignFunctionInterface #-}
module FormulaDll where

import Foreign.C.String( CString, CWString
                       , newCString, newCWString
                       , peekCString, peekCWString )

import Data.IORef

import qualified Data.Map as Map
import Foreign.Marshal.Alloc( free, malloc )
import Foreign.StablePtr
import EqManips.Types
import EqManips.InputParser.MathML
import EqManips.InputParser.EqCode
import EqManips.Algorithm.Eval
import EqManips.EvaluationContext
import EqManips.Algorithm.Utils
import EqManips.Renderer.Ascii
import EqManips.Renderer.RenderConf

dllRenderConf :: Bool -> Conf
dllRenderConf unicode = defaultRenderConf { useUnicode = unicode }

eqWDoForeign :: (String -> String) -> CWString -> IO CWString
eqWDoForeign f inCode = do
    inString <- peekCWString inCode
    newCWString $ f inString

eqDoForeign :: (String -> String) -> CString -> IO CString
eqDoForeign f inCode = do
    inString <- peekCString inCode
    newCString $ f inString

formatErrors :: Bool -> [(Formula TreeForm, String)] -> String
formatErrors unicode lst =
    foldr (\(f,s) acc -> s ++ "\n" ++ formatFormula conf f ++ acc) [] lst
        where conf = dllRenderConf unicode

eqFormulaParser :: Bool -> (Formula ListForm -> EqContext (Formula ListForm)) -> String
                -> String
eqFormulaParser unicode operation formulaText =
    either parseError computeFormula formulaList
        where formulaList = parseProgramm formulaText
              parseError err = "Error : " ++ show err
              computeFormula formulal = errs ++ finalForm
                  where rez = performLastTransformation 
                            $ mapM operation formulal

                        errs = formatErrors unicode $ errorList rez
                        finalForm = formatFormula (dllRenderConf unicode)
                                  . treeIfyFormula
                                  $ result rez

eqFormulaFormat :: Bool -> String -> String
eqFormulaFormat unicode formulaText = either parseError computeFormula formulaList
        where formulaList = parseProgramm formulaText
              parseError err = "Error : " ++ show err
              computeFormula = concatMap formater

              formater = formatFormula (dllRenderConf unicode)
                       . treeIfyFormula


eqMathMLTranslate :: CWString -> IO CWString
eqMathMLTranslate = eqWDoForeign $ mathMlToEqLang'

eqWEval :: CWString -> IO CWString
eqWEval = eqWDoForeign $ eqFormulaParser True evalGlobalLossyStatement

eqEval :: CString -> IO CString
eqEval = eqDoForeign $ eqFormulaParser False evalGlobalLossyStatement

eqFormat :: CString -> IO CString
eqFormat = eqDoForeign $ eqFormulaFormat False

eqWFormat :: CWString -> IO CWString
eqWFormat = eqWDoForeign $ eqFormulaFormat True

freeHaskell :: CWString -> IO ()
freeHaskell = free

eqCreateContext :: Int -> IO (StablePtr (IORef Context))
eqCreateContext _ = do
    ref <- newIORef Map.empty
    newStablePtr ref

eqDeleteContext :: StablePtr (IORef Context) -> IO ()
eqDeleteContext = freeStablePtr

type Context = Map.Map String (Formula ListForm)

eqFormulaParserC :: Bool -> (Formula ListForm -> EqContext (Formula ListForm))
                -> Context -> String
                -> (String, Context)
eqFormulaParserC unicode operation ctxt formulaText =
    either parseError computeFormula formulaList
        where formulaList = parseProgramm formulaText
              parseError err = ("Error : " ++ show err, ctxt)
              computeFormula formulal = (errs ++ finalForm, context rez)
                  where rez = performLastTransformationWithContext ctxt
                            $ mapM operation formulal

                        errs = formatErrors unicode $ errorList rez
                        finalForm = formatFormula (dllRenderConf unicode)
                                  . treeIfyFormula
                                  $ result rez

eqWEvalWithContext :: CWString -> StablePtr (IORef Context) -> IO CWString
eqWEvalWithContext str ref = do
    ref' <- deRefStablePtr ref
    context <- readIORef ref'
    haskString <- peekCWString str
    let (rez, context') = eqFormulaParserC True 
                                evalGlobalLossyStatement
                                context haskString
    writeIORef ref' context'
    newCWString rez
    
eqEvalWithContext :: CString -> StablePtr (IORef Context) -> IO CString
eqEvalWithContext str ref = do
    ref' <- deRefStablePtr ref
    context <- readIORef ref'
    haskString <- peekCString str
    let (rez, context') = eqFormulaParserC False 
                                evalGlobalLossyStatement
                                context haskString
    writeIORef ref' context'
    newCString rez

foreign export ccall "eqWFormat" eqWFormat :: CWString -> IO CWString
foreign export ccall "eqFormat" eqFormat :: CString -> IO CString
foreign export ccall "eqWEval" eqWEval :: CWString -> IO CWString
foreign export ccall "eqEval" eqEval :: CString -> IO CString
foreign export ccall "eqFreeHaskellString" freeHaskell :: CWString -> IO ()
foreign export ccall "eqMathMLTranslate" eqMathMLTranslate :: CWString -> IO CWString

foreign export ccall "eqCreateContext" eqCreateContext :: Int -> IO (StablePtr (IORef Context))
foreign export ccall "eqDeleteContext" eqDeleteContext :: (StablePtr (IORef Context)) -> IO ()
foreign export ccall "eqWEvalWithContext" eqWEvalWithContext :: CWString -> StablePtr (IORef Context)
                                                             -> IO CWString
foreign export ccall "eqEvalWithContext" eqEvalWithContext :: CString -> StablePtr (IORef Context)
                                                           -> IO CString

