module Language.Eq.InputParser.EqCode
    ( program  -- if you want to define some definition before
    , expr     -- if you want to evaluate just an expression
    , parseFormula
    , perfectParse 
    , parseProgramm
    ) where


import Control.Applicative( (<$>), (<*) )
{-import Data.Functor.Identity( Identity )-}

import Language.Eq.Types
import Language.Eq.Polynome
import Language.Eq.Linker
import Language.Eq.Algorithm.Utils

import Text.Parsec.Expr
import Text.Parsec
import Text.Parsec.Language( haskellStyle )
import qualified Text.Parsec.Token as P

-- | Helper function to parse a formula and apply all
-- needed algorithm to be able to apply them
parseFormula :: String -> Either ParseError (Formula ListForm)
parseFormula = either Left (Right . polynomizeFormula) . perfectParse

-- | Parse a formula and doesn't alter it's global form
-- (no polynomization)
perfectParse :: String -> Either ParseError (Formula ListForm)
perfectParse text = case runParser expr () "FromFile" text of
             Left e -> Left e
             Right f -> Right . listifyFormula
                              . linkFormula
                              $ Formula f

-- | Helper function to use to parse a programm.
-- Perform some transformations to get a usable
-- formula.
parseProgramm :: String -> Either ParseError [Formula ListForm]
parseProgramm text = rez
    where parsed = runParser program () "FromFile" text
          rez = case parsed of
                 Left a -> Left a
                 Right f -> Right $ map ( polynomizeFormula
                                        . listifyFormula
                                        . linkFormula
                                        . Formula ) f

-----------------------------------------------------------
--          Lexing defs
-----------------------------------------------------------
float :: Parsed st Double
float = P.float lexer

identifier :: Parsed st String
identifier = P.identifier lexer

reservedOp :: String -> Parsed st ()
reservedOp= P.reservedOp lexer

integer :: Parsed st Integer
integer = P.integer lexer

parens :: Parsec String u a -> Parsec String u a
parens = P.parens lexer

braces :: Parsec String u a -> Parsec String u a
braces = P.braces lexer

brackets :: Parsec String u a -> Parsec String u a
brackets = P.brackets lexer

stringVal :: Parsed st String
stringVal = P.stringLiteral lexer

whiteSpace :: Parsed st ()
whiteSpace = P.whiteSpace lexer

{-lexer :: P.GenTokenParser String st Identity-}
lexer  = P.makeTokenParser 
         (haskellStyle { P.reservedOpNames = [ "&", "|", "<", ">"
                                             , "*", "/", "+", "-"
                                             , "^", "=", "!", ":"
                                             , "_"
                                             ]
                       , P.identStart = letter
                       } )

-----------------------------------------------------------
--          Real "grammar"
-----------------------------------------------------------
type Parsed st b = Parsec String st b

program :: Parsed st [FormulaPrim]
program = sepBy expr (whiteSpace >> char ';' >> whiteSpace) <* whiteSpace
       <?> "program"

-- | Parser for the mini language is defined here
expr :: Parsed st FormulaPrim
expr = whiteSpace >> buildExpressionParser operatorDefs funCall
    <?> "expression"

{-operatorDefs :: OperatorTable String st Identity FormulaPrim-}
operatorDefs = 
    [ [postfix "!" (unOp OpFactorial)]
    , [prefix "-" (unOp OpNegate) ]
    , [binary "_" (\a b -> indexes a [b]) AssocLeft]
    , [binary "^" (binop OpPow) AssocLeft]
    , [binary "/" (binop OpDiv) AssocLeft, binary "*" (binop OpMul) AssocLeft]
    , [binary "+" (binop OpAdd) AssocLeft, binary "-" (binop OpSub) AssocLeft]
    , [binary "=" (binop OpEq)  AssocRight, binary "/=" (binop OpNe) AssocLeft
      ,binary "<" (binop OpLt)  AssocLeft,  binary ">"  (binop OpGt) AssocLeft
      ,binary "<=" (binop OpLe) AssocLeft,  binary ">=" (binop OpGe) AssocLeft]
    , [binary "&" (binop OpAnd) AssocLeft, binary "|" (binop OpOr) AssocLeft]
    , [binary "::" (binop OpCons) AssocRight]
    , [binary ":" (binop OpType) AssocLeft]
    , [ binary ":>" (binop OpLazyAttrib) AssocRight
      , binary ":=" (binop OpAttrib) AssocRight]
    ]

funCall :: Parsed st FormulaPrim
funCall = do
    caller <- term
    (app caller <$> argList) <|> return caller
        where argSeparator = whiteSpace >> char ',' >> whiteSpace
              exprList = sepBy expr argSeparator
              argList = parens (whiteSpace >> (exprList <* whiteSpace))

listParser :: Parsed st FormulaPrim
listParser = do
    lst <- brackets $ sepBy expr (whiteSpace >> char ',' >> whiteSpace) <* whiteSpace
    return $ list lst

variable :: Parsed st FormulaPrim
variable = Variable <$> identifier
        <?> "variable"

term :: Parsed st FormulaPrim
term = try trueConst
    <|> try falseConst
    <|> try nilConst
    <|> variable
    <|> try ellipses
    <|> try (CFloat <$> float)
    <|> (Variable <$> stringVal)
    <|> CInteger . fromInteger <$> integer
    <|> parens expr
    <|> meta Force <$> braces expr
    <|> listParser
    <?> "Term error"

ellipses :: Parsed st FormulaPrim
ellipses = return (NumEntity Ellipsis) <* (string "..." >> whiteSpace)

nilConst :: Parsed st FormulaPrim
nilConst = return (list []) <* (string "[]" >> whiteSpace)

trueConst :: Parsed st FormulaPrim
trueConst = return (Truth True) <* (string "true" >> whiteSpace)

falseConst :: Parsed st FormulaPrim
falseConst = return (Truth False) <* (string "false" >> whiteSpace)

-----------------------------------------------
----        Little helpers
-----------------------------------------------
{-binary :: String -> (a -> a -> a) -> Assoc -> Operator String st Identity a-}
binary name fun = Infix $ reservedOp name >> return fun

{-prefix :: String -> (a -> a) -> Operator String st Identity a-}
prefix  name fun       = Prefix $ reservedOp name >> return fun

{-postfix :: String -> (a -> a) -> Operator String st Identity a-}
postfix name fun = Postfix $ reservedOp name >> return fun

binop :: BinOperator -> FormulaPrim -> FormulaPrim -> FormulaPrim
binop op left right = binOp op [left, right]

