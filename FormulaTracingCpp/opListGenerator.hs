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

enumOpList :: String -> String
enumOpList enumName =
    "enum " ++ enumName ++ "\n{\n" ++ enumLst ++ "\n};\n"
        where enumLst = concat $
                  intersperse ",\n" [ '\t' : opName | (_,opName) <- unaryOp ++ opAtribList ++ operatorList]

compOverload :: String -> (String, String) -> (String, String)
compOverload className (overloaded, reprez) = (hFile, implFile)
    where hFile = concat ["inline bool operator ", overloaded, "( const ", className, "<Number> &op2 ) const;\n"]
          implFile = concat
              ["template<class Number>\n"
              ,"bool ", className, "<Number>::operator ", overloaded, "( const ", className, " &op2 ) const\n"
              ,"\t{ return number ", overloaded ," op2.number; };\n\n"
              ]
compNumber :: String -> (String, String) -> (String, String)
compNumber className (overloaded, reprez) = (hFile, implFile)
    where hFile = concat ["inline bool operator ", overloaded, "( Number op2 ) const;\n"]
          implFile = concat
              ["template<class Number>\n"
              ,"bool ", className, "<Number>::operator ", overloaded, "( Number op2 ) const\n"
              ,"\t{ return number ", overloaded ," op2; };\n\n"
              ]

binOpOverload :: String -> (String, String) -> (String, String)
binOpOverload className (overloaded, reprez) = (hFile, implFile)
    where hFile = concat ["inline ", className, "<Number> operator ", overloaded, "( const ", className, "<Number> &op2 ) const;\n"]
          implFile = concat
              ["template<class Number>\n"
              ,className, "<Number> ", className, "<Number>::operator ", overloaded, "( const ", className, " &op2 ) const\n"
              ,"{\n"
              ,"    FormulaNode<Number> *newFormula = new FormulaBiNode<Number>( ", reprez , ", formula, op2.formula );\n"
              ,"    return new ", className, "( newFormula, number ", overloaded ," op2.number );\n"
              ,"};\n\n"
              ]
    
binOpNumber :: String -> (String, String) -> (String, String)
binOpNumber className (overloaded, reprez) = (hFile, implFile)
    where hFile = concat ["inline ", className, "<Number> operator ", overloaded, "( Number op2 ) const;\n"]
          implFile = concat
              ["template<class Number>\n"
              ,className, "<Number> ", className, "<Number>::operator ", overloaded, "( Number op2 ) const\n"
              ,"{\n"
              ,"    FormulaNode<Number> *numberFormula = new FormulaNumber<Number>(op2);\n"
              ,"    FormulaNode<Number> *rez = new FormulaBiNode<Number>( ", reprez , ", formula, numberFormula );\n"
              ,"    return new ", className, "( rez, number ", overloaded ," op2 );\n"
              ,"};\n\n"
              ]

binOpNumberOverload :: String -> (String, String) -> (String, String)
binOpNumberOverload className (overloaded, reprez) = (hFile, implFile)
    where hFile = concat [ "template <class Number>\n"
                         , "inline ", className, "<Number> operator ", overloaded, "( Number op1, const ", className, "<Number> &op2 );\n"]
          implFile = concat
              ["template<class Number>\n"
              ,className, "<Number> operator ", overloaded, "( Number op1, const ", className, "<Number> &op2 )\n"
              ,"{\n"
              ,"    FormulaNode<Number> *numberFormula = new FormulaNumber<Number>(op1);\n"
              ,"    FormulaNode<Number> *rez = new FormulaBiNode<Number>( ", reprez , ", numberFormula, op2.formula );\n"
              , "   return new ", className, "( rez, op1 ", overloaded ," op2.number );\n"
              ,"};\n\n"
              ]

opAtribOverload :: String -> (String, String) -> (String, String)
opAtribOverload className (overloaded, reprez) = (hFile, implFile)
    where hFile = concat ["inline ", className, "<Number> operator ", overloaded, "( const ", className, "<Number> &op2 );\n"]
          implFile = concat
              ["template<class Number>\n"
              ,className, "<Number> ", className, "<Number>::operator ", overloaded, "( const ", className, " &op2 )\n"
              ,"{\n"
              ,"    formula = new FormulaBiNode<Number>( ", reprez, ", formula, op2.formula );\n"
              ,"    number ", overloaded, " op2.number;\n"
              ,"    return *this;\n"
              ,"};\n\n"
              ]

opAtribNumber :: String -> (String, String) -> (String, String)
opAtribNumber className (overloaded, reprez) = (hFile, implFile)
    where hFile = concat ["inline ", className, "<Number> operator ", overloaded, "( Number op2 );\n"]
          implFile = concat
              ["template<class Number>\n"
              ,className, "<Number> ", className, "<Number>::operator ", overloaded, "( Number op2 )\n"
              ,"{\n"
              ,"    FormulaNode<Number> numberFormula = new FormulaNumber<Number>( op2 );\n"
              ,"    formula = new FormulaBiNode<Number>( ", reprez, ", formula, numberFormula );\n"
              ,"    number ", overloaded, " op2;\n"
              ,"    return *this;\n"
              ,"};\n\n"
              ]

opAtribNumberOverload :: String -> (String, String) -> (String, String)
opAtribNumberOverload className (overloaded, reprez) = (hFile, implFile)
    where hFile = concat [ "template <class Number>\n"
                         , "inline Number operator ", overloaded, "( Number op1, const ", className, "<Number> &op2 );\n"]
          implFile = concat
              ["template<class Number>\n"
              ,"Number& operator ", overloaded, "( Number op1, const ", className, "<Number> &op2 )\n"
              ,"{\n"
              ,"    number ", overloaded, " op2.number;\n"
              ,"    return op1;\n"
              ,"};\n\n"
              ]


compNumberOverload :: String -> (String, String) -> (String, String)
compNumberOverload className (overloaded, reprez) = (hFile, implFile)
    where hFile = concat [ "template <class Number>\n"
                         , "inline bool operator ", overloaded, "( Number op1, const ", className, "<Number> &op2 );\n"]
          implFile = concat
              ["template<class Number>\n"
              ,"bool operator ", overloaded, "( Number op1, const ", className, "<Number> &op2 )\n"
              ,"{\n"
              ,"    return number ", overloaded, " op2.number;\n"
              ,"};\n\n"
              ]

toRender =
    [ (binOpOverload, operatorList)
    , (binOpNumber, operatorList)
    , (opAtribOverload, opAtribList)
    , (opAtribNumber, opAtribList)
    , (compOverload, compOperator)
    , (compNumber, compOperator)
    , (binOpNumberOverload, operatorList)
    , (opAtribNumberOverload, opAtribList)
    , (compNumberOverload, compOperator)
    ]

main :: IO ()
main = do
    let (header, impl) = unzip $ concatMap (\(f,lst) -> map (f "FormulaTracedNumber") lst) toRender
    putStr $ enumOpList "OpCode"
    putStr $ concat header
    putStr $ concat impl

