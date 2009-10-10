{-# OPTIONS_GHC -fno-warn-orphans #-}
module EqManips.InputParser.MathML ( mathMlToEqLang
                                   , mathMlToEqLang'
                                   ) where

import Data.Either
import Control.Applicative
import EqManips.Algorithm.Utils
import qualified EqManips.UnicodeSymbols as Uni

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
    | XunderOver ReducedXmlTree ReducedXmlTree ReducedXmlTree
    | Xrow [ReducedXmlTree]
    | Xtable [[ReducedXmlTree]]
    deriving (Show)

mathMlToEqLang' :: String -> String
mathMlToEqLang' txt = either (const "") id (mathMlToEqLang txt)

-- | Input XML code encoded in a string
-- output a string in Eq Language, ready to
-- be parsed by the usual meanings.
mathMlToEqLang :: String -> Either String String
mathMlToEqLang text =
    xmlParse' "mathml" text >>= simplifyXml >>= toProgramString

toProgramString :: ReducedXmlTree -> Either String String
toProgramString tree = (\s -> s "") <$> translate tree

simplifyXml :: Document a -> Either String ReducedXmlTree
simplifyXml (Document _ _ (Elem "math" _ lst) _) =
    Xrow <$> (eitherMap $ map simplifyContent lst)
simplifyXml _ = error "The xml document has the wrong format"

strOfContent :: Content a -> String
strOfContent (CString _ txt _) = txt
strOfContent _ = error "Xml string waited at this point"

elemOfContent :: Content a -> Element a
elemOfContent (CElem e _) = e
elemOfContent _ = error "Xml element waited at this point"

-- | Helper to simplify content
simplifyContent :: Content a -> Either String ReducedXmlTree
simplifyContent = simplify . elemOfContent

instance Applicative (Either a) where
    pure = Right
    (<*>) (Left a) _ = Left a
    (<*>) (Right _) (Left b) = Left b
    (<*>) (Right f) (Right v) = Right (f v)

instance Monad (Either a) where
    return = Right
    (>>=) (Left a) _ = Left a
    (>>=) (Right v) f = f v

eitherMap :: [Either a b] -> Either a [b]
eitherMap [] = Right []
eitherMap lst = foldr mapper (Right []) lst
    where mapper (Left a) _ = Left a
          mapper _ (Left a) = Left a
          mapper (Right v) (Right list) = Right (v:list)

-- | Really transform an XML file to a simplified tree
-- to make a better pattern matching
simplify :: Element a -> Either String ReducedXmlTree
simplify (Elem "mi" _ [c]) = Right . Xsymb $ strOfContent c
simplify (Elem "mn" _ [c]) = Right . Xnum $ strOfContent c
simplify (Elem "mo" _ [c]) = Right . Xop $ strOfContent c
simplify (Elem "mrow" _ lst) = Xrow <$> (eitherMap $ map simplifyContent lst)
simplify (Elem "msqrt" _ lst) = Xsqrt . Xrow <$> eitherMap (map simplifyContent lst)
simplify (Elem "mfrac" _ [a,b]) = Xfrac <$> simplifyContent a <*> simplifyContent b
simplify (Elem "msup" _ [a,b]) = Xsup <$> simplifyContent a <*> simplifyContent b
simplify (Elem "munderover" _ [a,b,c]) = 
    XunderOver <$> simplifyContent a <*> simplifyContent b <*> simplifyContent c

simplify (Elem "mtable" _ lst) = Xtable <$> lineList
    where lineList = eitherMap $ map (unrow . elemOfContent) lst

          unrow (Elem "mtr" _ cells) = eitherMap $ map (uncell . elemOfContent) cells
          unrow _ = Left "Ill formed MathML Matrix"

          uncell (Elem "mtd" _ [cell]) = simplifyContent cell
          uncell _ = Left "Ill format MathML Matrix cell"

simplify (Elem "mfenced" [ ("open", AttValue [Left "("])
                         , ("close", AttValue [Left "("]) ] lst) =
    Xrow <$> (eitherMap $ map simplifyContent lst)
    
simplify (Elem elemName _ _) = Left $ "Unknown MathMl element : " ++ elemName

str :: String -> String -> String
str = (++)

char :: Char -> String -> String
char = (:)

unicodeTranslation :: [(Int, String)]
unicodeTranslation =
    [ (Uni.logicalAnd, "&&")
    , (Uni.logicalOr, "||")
    , (Uni.identicalTo, "==")
    , (Uni.lessThanOrEqualTo, "<=")
    , (Uni.greaterThanOrEqualTo, ">=")
    ]

-- | Real transformation =)
translate :: ReducedXmlTree -> Either String ShowS
translate (Xop s) = Right $ str s
translate (Xsymb s) = Right $ str s
translate (Xnum s) = Right $ str s
translate (Xsqrt subTree) = (\sub -> str "sqrt(" . sub . char ')')
                         <$> translate subTree 
translate (Xfrac a b) = (\a' b' -> char '(' . a' . str ") / (" . b' . char ')')
                     <$> translate a 
                     <*> translate b
translate (Xsup a b) = (\a' b' -> char '(' . a' . str ") ^ (" . b' . char ')')
                    <$> translate a 
                    <*> translate b
translate (Xrow lst) =
    (\translist -> char '(' . concatS translist . char ')') <$> eitherMap (map translate lst)

translate (Xtable []) = Left "Wrong table format"
translate (Xtable lst) =
    (\elems -> str "matrix( " . shows lineCount . char ',' . shows columncount . char ','
                              . interspereseS (char ',') elems . char ')')
        <$> (eitherMap . map translate $ concat lst) 
    where lineCount = length lst
          columncount = length $ head lst
