{-# LANGUAGE ForeignFunctionInterface #-}
module FormulaDll where

import Foreign.C.String( CString, CWString
                       , newCString, newCWString
                       , peekCString, peekCWString )

import Foreign.Marshal.Alloc( free )
import EqManips.Types
import EqManips.InputParser.MathML
import EqManips.InputParser.EqCode
import EqManips.Algorithm.Eval
import EqManips.EvaluationContext
import EqManips.Algorithm.Utils
import EqManips.Renderer.Ascii
import EqManips.Renderer.RenderConf

dllRenderConf :: Conf
dllRenderConf = defaultRenderConf { useUnicode = True }

eqWDoForeign :: (String -> String) -> CWString -> IO CWString
eqWDoForeign f inCode = do
    inString <- peekCWString inCode
    newCWString $ f inString

eqDoForeign :: (String -> String) -> CString -> IO CString
eqDoForeign f inCode = do
    inString <- peekCString inCode
    newCString $ f inString

formatErrors :: [(Formula TreeForm, String)] -> String
formatErrors lst =
    foldr (\(f,s) acc -> s ++ "\n" ++ formatFormula dllRenderConf f ++ acc) [] lst

eqFormulaParser :: (Formula ListForm -> EqContext (Formula ListForm)) -> String -> String
eqFormulaParser operation formulaText =
    either parseError computeFormula formulaList
        where formulaList = parseProgramm formulaText
              parseError err = "Error : " ++ show err
              computeFormula formulal = errs ++ finalForm
                  where rez = performLastTransformation 
                            $ mapM operation formulal

                        errs = formatErrors $ errorList rez
                        finalForm = formatFormula dllRenderConf
                                  . treeIfyFormula
                                  $ result rez

eqMathMLTranslate :: CWString -> IO CWString
eqMathMLTranslate = eqWDoForeign $ mathMlToEqLang'

eqWEval :: CWString -> IO CWString
eqWEval = eqWDoForeign $ eqFormulaParser evalGlobalLossyStatement

eqEval :: CString -> IO CString
eqEval = eqDoForeign $ eqFormulaParser evalGlobalLossyStatement

freeHaskell :: CWString -> IO ()
freeHaskell = free

foreign export ccall "eqWEval" eqWEval :: CWString -> IO CWString
foreign export ccall "eqEval" eqEval :: CString -> IO CString
foreign export ccall "eqFreeHaskellString" freeHaskell :: CWString -> IO ()
foreign export ccall "eqMathMLTranslate" eqMathMLTranslate :: CWString -> IO CWString

