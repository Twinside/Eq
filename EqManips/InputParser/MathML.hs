module EqManips.InputParser.MathML ( mathMlToEqLang ) where

import EqManips.Types

import Text.XML.HaXml.Parse
import Text.XML.HaXml.Types

-- | Type used to reduce the complexity of XML
-- tree and favor an easier pattern matching
data ReducedXmlTree =
      Xop String
    | Xsymb String
    | Xnum String
    | Xrow [ReducedXmlTree]
    | Xsqrt [ReducedXmlTree]
    deriving (Show)

-- | Input XML code encoded in a string
-- output a string in Eq Language, ready to
-- be parsed by the usual meanings.
mathMlToEqLang :: String -> Maybe String
mathMlToEqLang _ = either (const Nothing) (const $ Just "")-- mayParse
    where mayParse = xmlParse' "mathml" fileContent

