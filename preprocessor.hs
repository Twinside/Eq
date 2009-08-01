module Preprocessor where

import System.FilePath
import Data.List

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
    , ( ".cc", cppLang)
    , ( ".cpp", cppLang)
    , ( ".h", cLang)
    , ( ".hpp", cppLang)

    , ( ".hs", haskellLang)
    , ( ".lhs", haskellLang)
    , ( ".ml", ocamlLang)
    , ( ".mli", ocamlLang)
    , ( ".py", shellLang)
    ]

beginResultMark, endResultMark :: String
beginResultMark = "<@<"
endResultMark = ">@>"

------------------------------------------------------
----    Choosing weapons for preprocessing
------------------------------------------------------
-- temp to avoid nasty warning
bidule :: FilePath -> String
bidule = takeExtension

------------------------------------------------------
----    Processing file's lines
------------------------------------------------------
eatSpaces :: String -> (String, String)
eatSpaces = eat []
    where eat acc (' ':xs) = eat (' ':acc) xs
          eat acc ('\t':xs) = eat ('\t':acc) xs
          eat acc xs = (acc, xs)

removeBeginComment :: LangDef -> String -> Maybe (String, String)
removeBeginComment langDef line = do
        let (iniSpace, restLine) = eatSpaces line
        rest <- stripPrefix (initComm langDef) restLine
        return (iniSpace ++ (initComm langDef), rest)

word :: String -> (String, String)
word = w []
    where w acc [] = (reverse acc, [])
          w acc (' ':xs) = (reverse acc, xs)
          w acc ('\t':xs) = (reverse acc, xs)
          w acc (c:xs) = w (c:acc) xs

data PreprocessState = PState (String -> PreprocessState) [String]
    
begin :: LangDef -> String -> PreprocessState
begin lang line =
    maybe (PState (begin lang) [line])
          (\(initSpace, line') -> rez initSpace . snd $ eatSpaces line')
          $ removeBeginComment lang line
        where rez initSpace ('E':'q':':':xs) =
                  let (command, rest) = word xs
                  in PState (gatherInput lang (initSpace, command, [rest])) [line]
              rez _ _ = PState (begin lang) [line]

              
gatherInput :: LangDef -> (String, String, [String]) -> String -> PreprocessState
gatherInput lang info@(initSpace, command, eqInfo) line = 
    maybe (PState (begin lang) $ produce lang info)
          markSearch
          $ removeBeginComment lang line
        where markSearch (_,line') = 
                maybe (PState (gatherInput lang (initSpace, command, eqInfo ++ [line])) [line])
                      (const $ PState (skip lang info) [])
                      $ stripPrefix beginResultMark line'

skip :: LangDef -> (String, String, [String]) -> String -> PreprocessState
skip lang info line =
    maybe (PState (skip lang info) [])
          (const . PState (begin lang) $ produce lang info)
          $ removeBeginComment lang line

produce :: LangDef -> (String, String, [String]) -> [String]
produce lang (initSpace, command, eqData) = [preLine, endLine]
    where bmark = initComm lang
          emark = endLineComm lang
          preLine = initSpace ++ bmark ++ beginResultMark ++ emark
          endLine = initSpace ++ bmark ++ endResultMark ++ emark

