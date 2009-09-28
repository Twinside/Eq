module Preprocessor( processFile ) where

import System.FilePath
import Data.List
import Control.Applicative
import EqManips.Algorithm.Eval
import EqManips.Algorithm.Utils
import EqManips.Renderer.Ascii
import EqManips.Renderer.Cpp
import EqManips.EvaluationContext
import EqManips.Types

data LangDef = LangDef {
          initComm :: String
        , endLineComm :: String
        , formater :: Formula -> [String]
    }


voidLang :: LangDef
voidLang = LangDef
    { initComm = ""
    , endLineComm = ""
    , formater = formulaTextTable 
    }

shellLang, cppLang, cLang, ocamlLang, haskellLang :: LangDef
cppLang = voidLang { initComm = "//", endLineComm = "", formater = (\f -> [convertToCpp f]) }
shellLang = voidLang { initComm = "#", endLineComm = "" }
cLang = voidLang { initComm = "/*", endLineComm = "*/" }
haskellLang = voidLang { initComm = "--", endLineComm = "" }
ocamlLang = voidLang { initComm = "(*", endLineComm = "*)" }

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

-- | Grab a word from a string, returning it and
-- the tail.
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

          spaceCount acc ' ' = 1 + acc
          spaceCount acc '\t' = 4 + acc
          spaceCount acc _ = acc

          unCommentedLine = replicate (foldl' spaceCount 0 initSpace) ' '

          process _ (Left err) = map (commentLine++) . lines $ show err
          process "format" (Right f) = printResult f
          process "eval" (Right f) = 
            let rez = performTransformation $ reduce f
            in case (errorList rez) of
                    [] -> reverse . map (unCommentedLine ++) . formater lang $ result rez
                    errs@(_:_) -> concat
                        [ (commentLine ++ txt ++ commentEnd) : printResult form
                                    | (form, txt) <- errs ]
          process _ (Right _) = ["Unknown command " ++ command]

          printResult f =
              reverse . map (\l -> commentLine ++ l ++ commentEnd) $ formulaTextTable f 

