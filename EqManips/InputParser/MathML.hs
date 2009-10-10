module EqManips.InputParser.MathML ( mathMlToEqLang
                                   , mathMlToEqLang'
                                   ) where

import EqManips.Types

import Text.XML.HaXml.Parse
import Text.XML.HaXml.Types

-- | Type used to reduce the complexity of XML
-- tree and favor an easier pattern matching
data ReducedXmlTree =
      Xop String
    | Xsymb String
    | Xnum String
    | Xsqrt ReducedXmlTree
    | Xfrac ReducedXmlTree ReducedXmlTree
    | Xsup ReducedXmlTree ReducedXmlTree
    | Xrow [ReducedXmlTree]
    deriving (Show)

mathMlToEqLang' :: String -> String
mathMlToEqLang' txt = maybe "" id (mathMlToEqLang txt)

-- | Input XML code encoded in a string
-- output a string in Eq Language, ready to
-- be parsed by the usual meanings.
mathMlToEqLang :: String -> Maybe String
mathMlToEqLang = either (const Nothing) 
                        (Just . toProgramString . simplifyXml)
               . xmlParse' "mathml"

toProgramString :: ReducedXmlTree -> String
toProgramString _ = ""

simplifyXml :: Document a -> ReducedXmlTree
simplifyXml (Document _ _ (Elem "math" _ lst) _) =
    Xrow $ map simplifyContent lst
simplifyXml _ = error "The xml document has the wrong format"

strOfContent :: Content a -> String
strOfContent (CString _ str _) = str
strOfContent _ = error "Xml string waited at this point"

elemOfContent :: Content a -> Element a
elemOfContent (CElem e _) = e
elemOfContent _ = error "Xml element waited at this point"

-- | Helper to simplify content
simplifyContent :: Content a -> ReducedXmlTree
simplifyContent = simplify . elemOfContent

-- | Really transform an XML file to a simplified tree
-- to make a better pattern matching
simplify :: Element a -> ReducedXmlTree
simplify (Elem "mi" _ [c]) = Xsymb $ strOfContent c
simplify (Elem "mn" _ [c]) = Xnum $ strOfContent c
simplify (Elem "mo" _ [c]) = Xop $ strOfContent c
simplify (Elem "mrow" _ lst) = Xrow $ map simplifyContent lst
simplify (Elem "msqrt" _ lst) = Xsqrt . Xrow $ map simplifyContent lst
simplify (Elem "mfrac" _ [a,b]) = Xfrac (simplifyContent a) (simplifyContent b)
simplify (Elem "msup" _ [a,b]) = Xsup (simplifyContent a) (simplifyContent b)
simplify (Elem elemName _ _) = error $ "Unknown MathMl element : " ++ elemName

str :: String -> String -> String
str = (++)

char :: Char -> String -> String
char = (:)

-- | Real transformation =)
translate :: ReducedXmlTree -> ShowS
translate (Xop s) = str s
translate (Xsymb s) = str s
translate (Xnum s) = str s
translate (Xsqrt subTree) = str "sqrt(" . translate subTree . char ')'
translate (Xfrac a b) =
    char '(' . translate a . str ") / (" . translate b . char ')'
translate (Xsup a b) =
    char '(' . translate a . str ") ^ (" . translate b . char ')'
translate (Xrow lst) = char '(' . concatMapS translate lst . char ')'

