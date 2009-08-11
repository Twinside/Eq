module Preprocessor where

import System.FilePath
import Data.List
import Control.Applicative
import EqManips.Algorithm.Eval
import EqManips.Algorithm.Utils
import EqManips.Renderer.Ascii
import EqManips.EvaluationContext

data LangDef = LangDef {
          initComm :: String
          , endLineComm :: String
    }

cppLang :: LangDef
cppLang = LangDef { initComm = "//", endLineComm = "" }

shellLang :: LangDef
shellLang = LangDef { initComm = "#", endLineComm = "" }

cLang :: LangDef
cLang = LangDef { initComm = "/*", endLineComm = "*/" }

haskellLang :: LangDef
haskellLang = LangDef { initComm = "--", endLineComm = "" }

ocamlLang :: LangDef
ocamlLang = LangDef { initComm = "(*", endLineComm = "*)" }

kindAssociation :: [(String, LangDef)]
kindAssociation =
    [ (".c", cLang)
    , ( ".C", cppLang)
    , ( ".cc", cppLang)
    , ( ".cpp", cppLang)
    , ( ".h", cLang)
    , ( ".hpp", cppLang)
    , ( ".java", cppLang)
    , ( ".cs", cppLang)

    , ( ".hs", haskellLang)
    , ( ".lhs", haskellLang)
    , ( ".ml", ocamlLang)
    , ( ".mli", ocamlLang)

    , ( ".py", shellLang)
    , ( ".rb", shellLang)
    , ( ".sh", shellLang)
    , ( ".ps1", shellLang)
    ]

beginResultMark, endResultMark :: String
beginResultMark = "<@<"
endResultMark = ">@>"

------------------------------------------------------
----    Choosing weapons for preprocessing
------------------------------------------------------
processFile :: FilePath -> IO String
processFile inFile =
    case langOfFileName inFile of
         Nothing -> do print "Error unrecognized file type"
                       return ""
         Just lang -> do
             file <- readFile inFile
             let rez = concat . obtainEqResult 
                              . processLines lang $ lines file
             return rez

-- temp to avoid nasty warning
langOfFileName :: FilePath -> Maybe LangDef
langOfFileName name = lookup (takeExtension name) kindAssociation

processLines :: LangDef -> [String] -> EqContext [String]
processLines lang lst = do
    fileLines' <- fileLines
    return . reverse . map (++ "\n") $ concat fileLines'
    where initVal = (PState (begin lang) (pure []), pure [])

          updater ((PState f _), acc) l = (rez , neoList)
                where rez = f l
                      (PState _ lst') = rez
                      neoList = do
                          a <- lst'
                          acc' <- acc
                          return $ a : acc'

          (_,fileLines) = foldl' updater initVal lst

------------------------------------------------------
----    Processing file's lines
------------------------------------------------------
eatSpaces :: String -> (String, String)
eatSpaces = eat []
    where eat acc (' ':xs) = eat (' ':acc) xs
          eat acc ('\t':xs) = eat ('\t':acc) xs
          eat acc xs = (acc, xs)

stripSuffix :: String -> String -> String
stripSuffix suffix text
    | isSuffixOf suffix text = take (length text - length suffix) text
    | otherwise = text
    
removeBeginComment :: LangDef -> String -> Maybe (String, String)
removeBeginComment langDef line = do
        let (iniSpace, restLine) = eatSpaces line
        rest <- stripPrefix (initComm langDef) restLine
        return ( iniSpace ++ (initComm langDef)
               , stripSuffix (endLineComm langDef) rest)

word :: String -> (String, String)
word = w []
    where w acc [] = (reverse acc, [])
          w acc (' ':xs) = (reverse acc, xs)
          w acc ('\t':xs) = (reverse acc, xs)
          w acc (c:xs) = w (c:acc) xs

data PreprocessState = PState (String -> PreprocessState) (EqContext [String])
    
begin :: LangDef -> String -> PreprocessState
begin lang line =
    maybe (PState (begin lang) $ pure [line])
          (\(initSpace, line') -> rez initSpace . snd $ eatSpaces line')
          $ removeBeginComment lang line
        where rez initSpace ('E':'q':':':xs) =
                  let (command, rest) = word xs
                  in PState (gatherInput lang (initSpace, command, [rest])) $ pure [line]
              rez _ _ = PState (begin lang) $ pure [line]

              
gatherInput :: LangDef -> (String, String, [String]) -> String -> PreprocessState
gatherInput lang info@(initSpace, command, eqInfo) line = 
    maybe (PState (begin lang) $ produce lang info >>= pure . (line:))
          markSearch
          $ removeBeginComment lang line
        where markSearch (_,line') = 
                maybe (PState (gatherInput lang (initSpace, command, eqInfo ++ [line'])) 
                              $ pure [line])
                      (const $ PState (skip lang info) $ pure [])
                      $ stripPrefix beginResultMark line'

skip :: LangDef -> (String, String, [String]) -> String -> PreprocessState
skip lang info line =
    maybe (PState (skip lang info) (pure []))
          endSearch
          $ removeBeginComment lang line
        where endSearch (_,line') =
                  maybe (PState (skip lang info) (pure []))
                        (const . PState (begin lang) $ produce lang info)
                        $ stripPrefix endResultMark line'

produce :: LangDef -> (String, String, [String]) -> EqContext [String]
produce lang (initSpace, command, eqData) =
   return $ endLine : process command mayParsedFormla ++ [preLine]
    where emark = endLineComm lang
          preLine = initSpace ++ beginResultMark ++ emark
          endLine = initSpace ++ endResultMark ++ emark

          mayParsedFormla = parseFormula $ concat eqData

          commentLine = initSpace ++ " "
          commentEnd = " " ++ emark

          process _ (Left err) = map (commentLine++) . lines $ show err
          process "format" (Right f) = printResult f
          process "eval" (Right f) = 
            let rez = performTransformation $ reduce f
            in case (errorList rez) of
                    [] -> printResult $ result rez
                    errs@(_:_) -> concat
                        [ (commentLine ++ txt ++ commentEnd) : printResult form
                                    | (form, txt) <- errs ]
          process _ (Right _) = ["Unknown command " ++ command]

          printResult f =
              reverse . map (\l -> commentLine ++ l ++ commentEnd) $ formulaTextTable f 

