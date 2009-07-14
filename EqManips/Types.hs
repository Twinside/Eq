module EqManips.Types( Formula( .. )
                     , BinOperator( .. )
                     , UnOperator( .. )
                     , prioOfBinaryOperators 
                     , expr 
                     , unOpNames
                     ) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language( haskellStyle )
import qualified Text.ParserCombinators.Parsec.Token as P
import Control.Applicative( (<$>) )

-- | All Binary operators
data BinOperator  =
    -- | '+'
    OpAdd  
    -- | '-'
    | OpSub 
    -- | '*'
    | OpMul 
    -- | '/'
    | OpDiv 
    -- | '^'
    | OpPow 
    deriving (Eq,Show,Read)

data UnOperator =
      OpNegate
    | OpAbs
    | OpSqrt

    | OpSin
    | OpASin

    | OpCos
    | OpACos

    | OpTan
    | OpATan

    | OpLn
    | OpLog

    | OpExp

    deriving (Eq, Show, Read)

unOpNames :: [(UnOperator, String)]
unOpNames =
    [ (OpNegate, "-")
    , (OpAbs, "abs")
    , (OpSqrt, "sqrt")

    , (OpSin, "sin")
    , (OpASin, "asin")

    , (OpCos, "cos")
    , (OpACos, "acos")

    , (OpTan, "tan")
    , (OpATan, "atan")

    , (OpLn, "ln")
    , (OpLog, "log")

    , (OpExp, "exp")
    ]

data Formula =
      Variable String
    | CInteger Int
    | CFloat Double
    -- | FunName arguments
    | App Formula [Formula]
    -- | LowBound highbound expression
    | Sum Formula Formula Formula
    -- | LowBound highbound expression
    | Product Formula Formula Formula
    -- | lowBound highBound expression dx
    | Integrate Formula Formula Formula Formula
    | UnOp UnOperator Formula
    | BinOp BinOperator Formula Formula
    -- | Width, Height, all formulas
    | Matrix Int Int [[Formula]]
    | Block Int Int Int
    deriving (Eq, Show, Read)

type Parsed a b = GenParser Char a b
    
prioOfBinaryOperators :: BinOperator -> Int
prioOfBinaryOperators = prio
    where prio OpAdd = 3
          prio OpSub = 3
          prio OpMul = 2
          prio OpDiv = 2
          prio OpPow = 1

-----------------------------------------------------------
--          Lexing defs
-----------------------------------------------------------
float :: CharParser st Double
float = P.float lexer

identifier :: CharParser st String
identifier = P.identifier lexer

reservedOp :: String -> CharParser st ()
reservedOp= P.reservedOp lexer

integer :: CharParser st Integer
integer = P.integer lexer

parens :: CharParser st a -> CharParser st a
parens = P.parens lexer

whiteSpace :: CharParser st ()
whiteSpace = P.whiteSpace lexer

lexer :: P.TokenParser st
lexer  = P.makeTokenParser 
         (haskellStyle { P.reservedOpNames = ["*","/","+","-","^"] } )

-----------------------------------------------------------
--          Real "grammar"
-----------------------------------------------------------
expr :: Parsed st Formula
expr = buildExpressionParser operatorDefs term
    <?> "expression"

funCall :: Formula -> Parsed st Formula
funCall funcName = App funcName <$> argList
       <?> "funCall"
        where argList = parens $ sepBy expr $ (char ',' >> whiteSpace)

var :: Parsed st Formula
var = Variable <$> identifier

term :: Parsed st Formula
term = parens expr
   <|> (do varName <- var
           try (funCall varName) <|> return varName)
   <|> try (CFloat <$> float)
   <|> CInteger . fromInteger <$> integer
   <?> "Term error"

binary :: String -> (a -> a -> a) -> Assoc -> Operator Char st a
binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc

prefix :: String -> (a -> a) -> Operator Char st a
prefix  name fun       = Prefix (do{ reservedOp name; return fun })

operatorDefs :: OperatorTable Char st Formula
operatorDefs = 
    [ [prefix "-" (UnOp OpNegate) ]
    , [binary "^" (BinOp OpPow) AssocLeft]
    , [binary "/" (BinOp OpDiv) AssocLeft, binary "*" (BinOp OpMul) AssocLeft]
    , [binary "+" (BinOp OpAdd) AssocLeft, binary "-" (BinOp OpSub) AssocLeft]
    ]

