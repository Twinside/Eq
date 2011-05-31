module Language.Eq.Preprocessor ( processFile
                             , LangDef( .. )
                             , kindAssociation
                             ) where

import System.FilePath
import Data.List
import Control.Applicative
import Text.Parsec.Error( ParseError )

import Language.Eq.Algorithm.Eval
import Language.Eq.Algorithm.Utils
import Language.Eq.InputParser.EqCode
import Language.Eq.Renderer.Ascii
import Language.Eq.Renderer.Cpp
import Language.Eq.EvaluationContext
import Language.Eq.Types
import Language.Eq.Renderer.RenderConf

data LangDef = LangDef {
          initComm :: String
        , languageName :: String
        , endLineComm :: String
        , formater :: Formula TreeForm -> [String]
    }


voidLang :: LangDef
voidLang = LangDef
    { initComm = ""
    , endLineComm = ""
    , languageName = ""
    , formater = formulaTextTable defaultRenderConf
    }

shellLang, cppLang, cLang, ocamlLang, haskellLang :: LangDef
cppLang = voidLang { initComm = "//"
                   , endLineComm = ""
                   , formater = (\f -> [convertToCpp f])
                   , languageName = "C++ like"
                   }

shellLang = voidLang { initComm = "#"
                     , endLineComm = ""
                     , languageName = "Shell like"
                     }

cLang = voidLang { initComm = "/*", endLineComm = "*/"
                 , languageName = "C like"}

haskellLang = voidLang { initComm = "--", endLineComm = ""
                       , languageName = "Haskell"
                       }

ocamlLang = voidLang { initComm = "(*", endLineComm = "*)"
                     , languageName = "OCaml" }

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
        return ( iniSpace ++ initComm langDef
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

-- Prelude const :: a -> b -> a
-- Prelude maybe :: b -> (a -> b) -> Maybe a -> b
-- Data.List stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
skip :: LangDef -> (String, String, [String]) -> String -> PreprocessState
skip lang info line =
    maybe (PState (skip lang info) (pure []))
          endSearch
          $ removeBeginComment lang line
        where endSearch (_,line') =
                  if stripPrefix endResultMark line' == Nothing
                      then PState (skip lang info) (pure [])
                      else PState (begin lang) $ produce lang info

produce :: LangDef -> (String, String, [String]) -> EqContext [String]
produce lang (initSpace, command, eqData) =
   return $ endLine : process command mayParsedFormla ++ [preLine]
    where emark = endLineComm lang
          preLine = initSpace ++ beginResultMark ++ emark
          endLine = initSpace ++ endResultMark ++ emark

          mayParsedFormla = parseFormula $ concat eqData

          commentLine = initSpace ++ " "
          commentEnd = ' ' : emark

          spaceCount acc ' ' = 1 + acc
          spaceCount acc '\t' = 4 + acc
          spaceCount acc _ = acc

          unCommentedLine = replicate (foldl' spaceCount 0 initSpace) ' '

          process :: String -> Either ParseError (Formula ListForm) -> [String]
          process _ (Left err) = map (commentLine++) . lines $ show err
          process "format" (Right f) = printResult (treeIfyFormula f)
          process "eval" (Right f) = 
            let rez = performTransformation $ reduce f
            in case (errorList rez) of
                    [] -> reverse . map (unCommentedLine ++) 
                                  . formater lang 
                                  . treeIfyFormula
                                  $ result rez
                    errs@(_:_) -> concat
                        [ (commentLine ++ txt ++ commentEnd) : printResult form
                                    | (form, txt) <- errs ]
          process _ (Right _) = ["Unknown command " ++ command]

          printResult =
              reverse . map (\l -> commentLine ++ l ++ commentEnd)
                      . formulaTextTable defaultRenderConf
                      


