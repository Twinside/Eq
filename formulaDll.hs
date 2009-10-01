{-# LANGUAGE ForeignFunctionInterface #-}
module FormulaDll where

import Foreign.C.String

import EqManips.Types
import EqManips.Algorithm.Eval
import EqManips.EvaluationContext
import EqManips.Algorithm.Utils
import EqManips.Renderer.Ascii

eqDoForeign :: (String -> String) -> CWString -> IO CWString
eqDoForeign f inCode = do
    inString <- peekCWString inCode
    newCWString $ f inString

formatErrors :: [(Formula, String)] -> String
formatErrors lst =
    foldr (\(f,s) acc -> s ++ "\n" ++ formatFormula f ++ acc) [] lst

eqFormulaParser :: (Formula -> EqContext Formula) -> String -> String
eqFormulaParser operation formulaText =
    either parseError computeFormula formulaList
        where formulaList = parseProgramm formulaText
              parseError err = "Error : " ++ show err
              computeFormula formulal = errs 
                  where rez = performLastTransformation 
                            $ mapM operation formulal

                        errs = formatErrors $ errorList rez
                        finalForm = formatFormula $ result rez

eqEval :: CWString -> IO CWString
eqEval = eqDoForeign $ eqFormulaParser evalGlobalLossyStatement


foreign export ccall "eqEval" eqEval :: CWString -> IO CWString

