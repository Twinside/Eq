import Data.List

operatorList =
    [("+", "OpAdd")
    ,("-", "OpSub")
    ,("*", "OpMul")
    ,("/", "OpDiv")
    ,("%", "OpMod")
    ]

opAtribList = [("+=","OpAdd")
              ,("-=","OpSub")
              ,("*=","OpMul")
              ,("/=","OpDiv")
              ,("%=","OpMod")
              ]

compOperator = [("<", "OpLT")
               ,("<=","OpLE")
               ,(">=","OpGE")
               ,(">", "OpGT")
               ,("==","OpEQ")
               ,("!=","OpNE")
               ]

unaryOp = [("-", "OpNegate")
          ,("++", "OpSucc")
          ,("--", "OpPred")
          ]

mathFun =
    [ ("abs", "OpAbs")
	, ("acos", "OpAcos")
	, ("asin", "OpAsin")
	, ("atan", "OpAtan")
	, ("ceil", "OpCeil")
	, ("cos", "OpCos")
	, ("cosh", "OpCosh")
	, ("exp", "OpExp")
	, ("fabs", "OpAbs")
	, ("floor", "OpFloor")
	, ("log", "OpLog")
	, ("log10", "OpLog10")
	, ("pow", "OpPow")
	, ("sin", "OpSin")
	, ("sinh", "OpSinh")
	, ("sqrt", "OpSqrt")
	, ("tan", "OpTan")
	, ("tanh", "OpTanh")
	{-, ("div", "OpDiv")-}
	{-, ("ldiv", "OpLDiv")-}
    ]

dontKnowWhatToDoWithThese =
	[ ("labs", "")
	, ("atan2", "")
	, ("ldexp", "")
	, ("frexp", "")
	, ("fmod", "")
	, ("modf", "")
    ]


enumOpList :: String -> ([String], [String])
enumOpList enumName =
   (["enum " ++ enumName ++ "\n"
    ,"{\n"] ++ enumLst ++ ["\n};\n"], initializer)
        where eqSnd (_,e) (_,e') = e == e'
              opList = nubBy eqSnd $ unaryOp ++ operatorList ++ mathFun
              initializer = [ "const char   *opReprez[] =\n"
                            , "\t{ " ++ concat (intersperse ",\n" [ "\t\" " ++ opReprez ++ " \"" | (opReprez,_) <- opList ])
                            , "\n\t};"
                            ]
              enumLst = intersperse ",\n" [ '\t' : opName | (_,opName) <- opList ]

mathFunOverload :: String -> (String, String) -> ([String], [String])
mathFunOverload className (overloaded, code) = (hFile, implFile)
    where hFile = ["template <class Number>\n"
                  ,className ++ "<Number> " ++ overloaded ++ "( const " ++ className ++ "<Number> &val );\n"]
          implFile = ["template <class Number>\n"
                     ,className ++ "<Number> " ++ overloaded ++ "( const " ++ className ++ "<Number> &val )\n"
                     ,"{\n"
                     ,"\tFormulaNode<Number> *newFormula = new FormulaUnode<Number>( " ++ code ++ ", val.getFormula() );\n"
                     ,"\t" ++ className ++ "<Number> ret(newFormula, std::" ++ overloaded ++ "(val.getNumber()) );\n"
                     ,"\treturn ret;"
                     ,"};\n"
                     ]

compOverload :: String -> (String, String) -> ([String], [String])
compOverload className (overloaded, reprez) = (hFile, implFile)
    where hFile = ["inline bool operator " ++ overloaded ++ "( const ", className, "<Number> &op2 ) const;\n"]
          implFile =
              ["template<class Number>\n"
              ,"bool " ++ className ++ "<Number>::operator " ++ overloaded ++ "( const " ++ className ++ "<Number> &op2 ) const\n"
              ,"\t{ return number " ++ overloaded ++ " op2.number; };\n\n"
              ]

compNumber :: String -> (String, String) -> ([String], [String])
compNumber className (overloaded, reprez) = (hFile, implFile)
    where hFile = ["inline bool operator " ++ overloaded ++ "( Number op2 ) const;\n"]
          implFile =
              ["template<class Number>\n"
              ,"bool " ++ className ++ "<Number>::operator " ++ overloaded ++ "( Number op2 ) const\n"
              ,"\t{ return number " ++ overloaded ++ " op2; };\n\n"
              ]

binOpOverload :: String -> (String, String) -> ([String], [String])
binOpOverload className (overloaded, reprez) = (hFile, implFile)
    where hFile = ["inline ", className, "<Number> operator ", overloaded, "( const ", className, "<Number> &op2 ) const;\n"]
          implFile =
              ["template<class Number>\n"
              ,className ++ "<Number> " ++ className ++ "<Number>::operator " ++ overloaded ++ "( const " ++ className ++ "<Number> &op2 ) const\n"
              ,"{\n"
              ,"    FormulaNode<Number> *newFormula = new FormulaBiNode<Number>( " ++ reprez ++ ", formula, op2.formula );\n"
              ,"    " ++ className ++ "<Number> ret(newFormula, number " ++ overloaded ++ " op2.number );\n"
              ,"    return ret;\n"
              ,"};\n\n"
              ]
    
binOpNumber :: String -> (String, String) -> ([String], [String])
binOpNumber className (overloaded, reprez) = (hFile, implFile)
    where hFile = ["inline ", className, "<Number> operator ", overloaded, "( Number op2 ) const;\n"]
          implFile =
              ["template<class Number>\n"
              ,className ++ "<Number> " ++ className ++ "<Number>::operator " ++ overloaded ++ "( Number op2 ) const\n"
              ,"{\n"
              ,"    FormulaNode<Number> *numberFormula = new FormulaNumber<Number>(op2);\n"
              ,"    FormulaNode<Number> *rez = new FormulaBiNode<Number>( " ++ reprez ++ ", formula, numberFormula );\n"
              ,"    " ++ className ++ "<Number> ret(rez, number " ++ overloaded ++ " op2 );\n"
              ,"    return ret;\n"
              ,"};\n\n"
              ]

binOpNumberOverload :: String -> (String, String) -> ([String], [String])
binOpNumberOverload className (overloaded, reprez) = (hFile, implFile)
    where hFile = [ "template <class Number>\n"
                  , "inline " ++ className ++ "<Number> operator " ++ overloaded ++ "( Number op1, const " ++ className ++ "<Number> &op2 );\n"]
          implFile =
              ["template<class Number>\n"
              ,className ++ "<Number> operator " ++ overloaded ++ "( Number op1, const " ++ className ++ "<Number> &op2 )\n"
              ,"{\n"
              ,"    FormulaNode<Number> *numberFormula = new FormulaNumber<Number>(op1);\n"
              ,"    FormulaNode<Number> *rez = new FormulaBiNode<Number>( " ++ reprez ++ ", numberFormula, op2.getFormula() );\n"
              ,"    " ++ className ++ "<Number> ret(rez, op1 " ++ overloaded ++ " op2.getNumber() );\n"
              ,"    return ret;\n"
              ,"};\n\n"
              ]

opAtribOverload :: String -> (String, String) -> ([String], [String])
opAtribOverload className (overloaded, reprez) = (hFile, implFile)
    where hFile = ["inline " ++ className ++ "<Number> operator " ++ overloaded ++ "( const " ++ className ++ "<Number> &op2 );\n"]
          implFile =
              ["template<class Number>\n"
              ,className ++ "<Number> " ++ className ++ "<Number>::operator " ++ overloaded ++ "( const " ++ className ++ "<Number> &op2 )\n"
              ,"{\n"
              ,"    FormulaNode<Number> *oldFormula = formula;\n"
              ,"    formula = new FormulaBiNode<Number>( " ++ reprez ++ ", oldFormula, op2.formula );\n"
              ,"    oldFormula->release();\n"
              ,"    formula->acquire();\n"
              ,"    number " ++ overloaded ++ " op2.number;\n"
              ,"    return *this;\n"
              ,"};\n\n"
              ]

opAtribNumber :: String -> (String, String) -> ([String], [String])
opAtribNumber className (overloaded, reprez) = (hFile, implFile)
    where hFile = ["inline " ++ className ++ "<Number> operator " ++ overloaded ++ "( Number op2 );\n"]
          implFile =
              ["template<class Number>\n"
              ,className ++ "<Number> " ++ className ++ "<Number>::operator " ++ overloaded ++ "( Number op2 )\n"
              ,"{\n"
              ,"    FormulaNode<Number> *oldFormula = formula;\n"
              ,"    FormulaNode<Number> numberFormula = new FormulaNumber<Number>( op2 );\n"
              ,"    formula = new FormulaBiNode<Number>( " ++ reprez ++ ", oldFormula, numberFormula );\n"
              ,"    oldFormula->release();\n"
              ,"    formula->acquire();\n"
              ,"    number " ++ overloaded ++ " op2;\n"
              ,"    return *this;\n"
              ,"};\n\n"
              ]

opAtribNumberOverload :: String -> (String, String) -> ([String], [String])
opAtribNumberOverload className (overloaded, reprez) = (hFile, implFile)
    where hFile = [ "template <class Number>\n"
                  , "inline Number operator " ++ overloaded ++ "( Number op1, const " ++ className ++ "<Number> &op2 );\n"]
          implFile =
              ["template<class Number>\n"
              ,"Number operator " ++ overloaded ++ "( Number op1, const " ++ className ++ "<Number> &op2 )\n"
              ,"{\n"
              ,"    op1 " ++ overloaded ++ " op2.getNumber();\n"
              ,"    return op1;\n"
              ,"};\n\n"
              ]


compNumberOverload :: String -> (String, String) -> ([String], [String])
compNumberOverload className (overloaded, reprez) = (hFile, implFile)
    where hFile = [ "template <class Number>\n"
                  , "inline bool operator " ++ overloaded ++ "( Number op1, const " ++ className ++ "<Number> &op2 );\n"]
          implFile =
              ["template<class Number>\n"
              ,"bool operator " ++ overloaded ++ "( Number op1, const " ++ className ++ "<Number> &op2 )\n"
              ,"{\n"
              ,"    return op1 " ++ overloaded ++ " op2.number;\n"
              ,"};\n\n"
              ]

inClassRender =
    [ (binOpOverload, operatorList)
    , (binOpNumber, operatorList)
    , (opAtribOverload, opAtribList)
    , (opAtribNumber, opAtribList)
    , (compOverload, compOperator)
    , (compNumber, compOperator)
    ]

outClassRender =
    [ (binOpNumberOverload, operatorList)
    , (opAtribNumberOverload, opAtribList)
    , (compNumberOverload, compOperator)
    , (mathFunOverload, mathFun)
    ]

addTab :: [[String]] -> [String]
addTab lst =
    concat [ ['\t' : str | str <- stringList] | stringList <- lst ]

addNamespace :: String -> [String] -> [String]
addNamespace namespace content =
    ("namespace " ++ namespace ++ "\n")
    : "{\n"
    : content
    ++ ["}\n"]

addGuards macro content = 
   ["#ifndef ", macro, "\n"
   ,"#define ", macro, "\n"
   ,"\n"]
   ++ content ++
   ["\n#endif /* ", macro, " */\n\n"]

addHeaderFioritures macro namespace =
    addGuards macro . addNamespace namespace 

main :: IO ()
main = do
    let render = unzip . concatMap (\(f,lst) -> map (f "FormulaTracedNumber") lst)
        (header, impl) = render inClassRender
        (outHeader, outImpl) = render outClassRender
        (enumHeader, enumImpl) = enumOpList "OpCode"
    writeFile "enumOpCode.h" . concat 
                             . addHeaderFioritures "__ENUMOPCODE_H__" "Formula" 
                             $ enumHeader

    writeFile "FormulaTables.cpp" . concat
                                  . addNamespace "Formula"
                                  $ enumImpl

    writeFile "FormulaBoileprlateOverloaded.h" . concat 
                             . addGuards "__FORMULA_BOILERPLATE_OPERATOR_OVERLOADING_H__" 
                             $ concat header

    writeFile "Formula.inline.h" . concat 
                                 . addHeaderFioritures "__FORMULA_INLINE_H__" "Formula" 
                                 $ addTab (impl ++ outImpl)
    writeFile "FormulaOverloadedOp.h" 
                               . concat 
                               . addHeaderFioritures "__FORMULA_OVERLOADED_OP_H__" "Formula" 
                               $ addTab outHeader


