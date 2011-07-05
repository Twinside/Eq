{-# LANGUAGE ForeignFunctionInterface #-}
module FormulaDll where

import Foreign.C.String( CString, CWString
                       , newCString, newCWString
                       , peekCString, peekCWString )

import Data.IORef
import Data.List
import qualified Data.Map as Map
import Foreign.Marshal.Alloc( free, malloc )
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Language.Eq.Linker
import Language.Eq.Types
import Language.Eq.InputParser.MathML
import Language.Eq.InputParser.EqCode
import Language.Eq.Algorithm.Eval
import Language.Eq.EvaluationContext
import Language.Eq.Algorithm.Utils
import Language.Eq.Renderer.Ascii
import Language.Eq.Renderer.EqCode
import Language.Eq.Renderer.Mathml
import Language.Eq.Renderer.RenderConf
import Language.Eq.BaseLibrary

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

eqCreateContextWithBaseLibrary :: Int -> IO (StablePtr (IORef Context))
eqCreateContextWithBaseLibrary _ = do
    ref <- newIORef defaultSymbolTable
    newStablePtr ref
eqDeleteContext :: StablePtr (IORef Context) -> IO ()
eqDeleteContext = freeStablePtr

type Context = Map.Map String (Formula ListForm)

eqFormulaParserC :: Bool -> (Formula ListForm -> EqContext (Formula ListForm))
                -> Context -> String
                -> (String, String, String, Context)
eqFormulaParserC unicode operation ctxt formulaText =
    either parseError computeFormula formulaList
        where formulaList = parseProgramm formulaText
              parseError err = ("Error : " ++ show err, "", "", ctxt)
              computeFormula formulal = ( errs ++ finalForm
                                        , unparse . unTagFormula $ treeified
                                        , mathmlRender defaultRenderConf $ treeified
                                        , context rez)
                  where rez = performLastTransformationWithContext ctxt
                            $ mapM operation formulal

                        errs = formatErrors unicode $ errorList rez
                        treeified = treeIfyFormula $ result rez
                        finalForm = formatFormula (dllRenderConf unicode) $ treeified 

eqWEvalWithContext :: CWString -> StablePtr (IORef Context) 
                               -> Ptr CWString
                               -> Ptr CWString
                               -> Ptr CWString
                               -> IO ()
eqWEvalWithContext str ref rezPtr unparsedPtr mathMLPtr = do
    ref' <- deRefStablePtr ref
    context <- readIORef ref'
    haskString <- peekCWString str
    let (rez, unparsed, mathml, context') = 
            eqFormulaParserC True evalGlobalLossyStatement context haskString
    writeIORef ref' context'
    
    poke rezPtr =<< newCWString rez
    poke unparsedPtr =<< newCWString unparsed
    poke mathMLPtr =<< newCWString mathml
    
eqEvalWithContext :: CString -> StablePtr (IORef Context)
                             -> Ptr CString
                             -> Ptr CString
                             -> Ptr CString
                             -> IO ()
eqEvalWithContext str ref rezPtr unparsedPtr mathMLPtr = do
    ref' <- deRefStablePtr ref
    context <- readIORef ref'
    haskString <- peekCString str
    let (rez, unparsed, mathml, context') = 
            eqFormulaParserC True evalGlobalLossyStatement context haskString
    writeIORef ref' context'
    
    poke rezPtr =<< newCString rez
    poke unparsedPtr =<< newCString unparsed
    poke mathMLPtr =<< newCString mathml

exportStringList :: [String] -> IO (Ptr CString)
exportStringList lst = newArray =<< mapM newCString lst

getEntityList :: Int -> IO (Ptr CString)
getEntityList _ = exportStringList $ map fst entityList

getMetaFunctionList :: Int -> IO (Ptr CString)
getMetaFunctionList _ = exportStringList $ map fst metaFunctionList

getUnaryFunctionList :: Int -> IO (Ptr CString)
getUnaryFunctionList _ = exportStringList $ map fst unaryFunctions

foreign export ccall "getEntityList" getEntityList :: Int -> IO (Ptr CString)
foreign export ccall "getMetaFunctionList" getMetaFunctionList :: Int -> IO (Ptr CString)
foreign export ccall "getUnaryFunctionList" getUnaryFunctionList :: Int -> IO (Ptr CString)

foreign export ccall "eqWFormat" eqWFormat :: CWString -> IO CWString
foreign export ccall "eqFormat" eqFormat :: CString -> IO CString
foreign export ccall "eqWEval" eqWEval :: CWString -> IO CWString
foreign export ccall "eqEval" eqEval :: CString -> IO CString
foreign export ccall "eqFreeHaskellString" freeHaskell :: CWString -> IO ()
foreign export ccall "eqMathMLTranslate" eqMathMLTranslate :: CWString -> IO CWString

foreign export ccall "eqCreateContextWithBaseLibrary" eqCreateContextWithBaseLibrary :: Int -> IO (StablePtr (IORef Context))
foreign export ccall "eqCreateContext" eqCreateContext :: Int -> IO (StablePtr (IORef Context))
foreign export ccall "eqDeleteContext" eqDeleteContext :: (StablePtr (IORef Context)) -> IO ()
foreign export ccall "eqWEvalWithContext" eqWEvalWithContext :: CWString -> StablePtr (IORef Context)
                                                             -> Ptr CWString -> Ptr CWString -> Ptr CWString
                                                             -> IO ()

foreign export ccall "eqEvalWithContext" eqEvalWithContext :: CString -> StablePtr (IORef Context)
                                                           -> Ptr CString -> Ptr CString -> Ptr CString
                                                           -> IO ()
