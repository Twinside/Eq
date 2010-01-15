module EqManips.InputParser.EqCode
    ( program  -- if you want to define some definition before
    , expr     -- if you want to evaluate just an expression
    , parseFormula
    , perfectParse 
    , parseProgramm
    ) where


import Control.Applicative( (<$>), (<*) )
import Control.Monad.Identity

import EqManips.Types
import EqManips.Polynome
import EqManips.Linker
import EqManips.Algorithm.Utils

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

parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = P.parens lexer

whiteSpace :: Parsed st ()
whiteSpace = P.whiteSpace lexer

lexer :: P.GenTokenParser String st Identity
lexer  = P.makeTokenParser 
         (haskellStyle { P.reservedOpNames = [ "&", "|", "<", ">"
                                             , "*", "/", "+", "-"
                                             , "^", "=", "!", ":"] } )

-----------------------------------------------------------
--          Real "grammar"
-----------------------------------------------------------
type Parsed st b = ParsecT String st Identity b

program :: Parsed st [FormulaPrim]
program = sepBy expr (whiteSpace >> char ';' >> whiteSpace) <* whiteSpace
       <?> "program"

-- | Parser for the mini language is defined here
expr :: Parsed st FormulaPrim
expr = whiteSpace >> buildExpressionParser operatorDefs funCall
    <?> "expression"

operatorDefs :: OperatorTable String st Identity FormulaPrim
operatorDefs = 
    [ [postfix "!" (unOp OpFactorial)]
    , [prefix "-" (unOp OpNegate) ]
    , [binary "^" (binop OpPow) AssocLeft]
    , [binary "/" (binop OpDiv) AssocLeft, binary "*" (binop OpMul) AssocLeft]
    , [binary "+" (binop OpAdd) AssocLeft, binary "-" (binop OpSub) AssocLeft]
    , [binary "=" (binop OpEq)  AssocRight, binary "/=" (binop OpNe) AssocLeft
      ,binary "<" (binop OpLt)  AssocLeft,  binary ">"  (binop OpGt) AssocLeft
      ,binary "<=" (binop OpLe) AssocLeft,  binary ">=" (binop OpGe) AssocLeft]
    , [binary "&" (binop OpAnd) AssocLeft, binary "|" (binop OpOr) AssocLeft]
    , [binary ":>" (binop OpLazyAttrib) AssocRight, binary ":=" (binop OpAttrib) AssocRight]
    ]

funCall :: Parsed st FormulaPrim
funCall =  do
    caller <- term
    (app caller <$> argList) <|> return caller
        where argSeparator = whiteSpace >> char ',' >> whiteSpace
              exprList = sepBy expr argSeparator
              argList = parens (whiteSpace >> (exprList <* whiteSpace))

variable :: Parsed st FormulaPrim
variable = Variable <$> identifier
        <?> "variable"

term :: Parsed st FormulaPrim
term = try trueConst
    <|> try falseConst
    <|> variable
    <|> try (CFloat <$> float)
    <|> CInteger . fromInteger <$> integer
    <|> parens expr
    <?> "Term error"

trueConst :: Parsed st FormulaPrim
trueConst = return (Truth True) <* (string "true" >> whiteSpace)

falseConst :: Parsed st FormulaPrim
falseConst = return (Truth False) <* (string "false" >> whiteSpace)

-----------------------------------------------
----        Little helpers
-----------------------------------------------
binary :: String -> (a -> a -> a) -> Assoc -> Operator String st Identity a
binary name fun = Infix (do{ reservedOp name; return fun })

prefix :: String -> (a -> a) -> Operator String st Identity a
prefix  name fun       = Prefix (do{ reservedOp name; return fun })

postfix :: String -> (a -> a) -> Operator String st Identity a
postfix name fun = Postfix (do{ reservedOp name; return fun })

binop :: BinOperator -> FormulaPrim -> FormulaPrim -> FormulaPrim
binop op left right = binOp op [left, right]

