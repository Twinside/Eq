module EqManips.DocBookGenerator( generateDocBook ) where

import EqManips.Algorithm.Utils
import EqManips.Linker

str :: String -> ShowS
str = (++)

tagger :: String -> ShowS -> ShowS
tagger tagName body =
    str ('<' : tagName ++ ">") . body . str ("</" ++ tagName ++ ">")

taggerEndl :: String -> ShowS -> ShowS
taggerEndl tagName body =
    str ("\n<" ++ tagName ++ ">\n") . body . str ("\n</" ++ tagName ++ ">\n")

refentry :: String -> ShowS -> ShowS
refentry id body = str "<refentry xml:id=\"" . str id . str "\">\n"
                 . body
                 . str "</refentry>\n"

article :: ShowS -> ShowS
article inner = str "<article xmlns='http://docbook.org/ns/docbook'>\n"
              . inner
              . str "</article>\n"

refnamediv, refname, refpurpose, funcprototype, paramdef
    , funcdef, refsect1, title, para , function
    :: ShowS -> ShowS
refnamediv    = taggerEndl "refnamediv"
refname       = tagger "refname"
refpurpose    = tagger "refpurpose"
funcprototype = tagger "funcprototype "
paramdef      = tagger "paramdef"
funcdef       = tagger "funcdef"
refsect1      = taggerEndl "refsect1"
title         = taggerEndl "title"
para          = taggerEndl "para"
function      = tagger "function"
indexterm     = tagger "indexterm"
primary       = tagger "primary"
secondary     = tagger "secondary"
parameter     = tagger "parameter"
refsynopsis   = tagger "refsynopsis"

entityToDocBook :: (String, (DocString, LongDescr, a)) -> ShowS
entityToDocBook (entityName, (liteInfo, longInfo, _)) = refentry entityName $
    refnamediv ( refname (str entityName)
               . refpurpose (str liteInfo))
  . refsect1 ( indexterm (primary (str "constant") . secondary (str entityName))
             . str longInfo
             )


unaryFunToDocBook :: (String, (DocString, LongDescr, a)) -> ShowS
unaryFunToDocBook (funName, (liteInfo, longInfo, _)) = refentry funName $
    refnamediv ( refname (str funName)
               . refpurpose (str liteInfo) )
  . refsynopsis ( funcprototype ( funcdef (str "formula")
                                . paramdef (parameter $ str "x")
                                )
                )
  . refsect1 ( title (str "Description")
             . para ( indexterm (primary (str "function") . secondary (str funName))
                    . str longInfo )
             )

multiParamFunToDocBook :: (String, (DocString, LongDescr, [(DocString,LongDescr)], a)) -> ShowS
multiParamFunToDocBook (funName, (liteInfo, longInfo, params, _)) =
    refnamediv ( refname (str funName)
               . refpurpose (str liteInfo) )
  . refsynopsis ( funcprototype ( funcdef (str "formula")
                                . concatMapS (paramdef . parameter . str . fst) params
                                )
                )
  . refsect1 ( title (str "Description")
             . para ( indexterm (primary (str "function") . secondary (str funName))
                    . str longInfo )
             )

generateDocBook :: ShowS
generateDocBook =
    str "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n"
    . str "<!DOCTYPE article PUBLIC \"-//OASIS//DTD DocBook XML V4.3//EN\" \"http://www.oasis-open.org/docbook/xml/4.3/docbookx.dtd\">"
    . article ( concatMapS entityToDocBook entityList
              . concatMapS unaryFunToDocBook (metaFunctionList ++ unaryFunctions) 
              . concatMapS multiParamFunToDocBook multiParamsFunctions
              )

