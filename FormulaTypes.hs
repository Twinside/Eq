module FormulaTypes( Formula( .. )
                   , BinOperator( .. )
                   , UnOperator( .. )
                   , expr 
                   ) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language( haskellStyle )
import qualified Text.ParserCombinators.Parsec.Token as P
import Control.Applicative( (<$>) )
import Control.Monad( liftM2 )

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
    deriving (Eq, Show, Read)

data Formula =
      Variable String
    | CInteger Int
    | CFloat Double
    | App Formula [Formula]
    | UnOp UnOperator Formula
    | BinOp BinOperator Formula Formula
    deriving (Eq, Show, Read)

type Parsed a b = GenParser Char a b

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

lexer :: P.TokenParser st
lexer  = P.makeTokenParser 
         (haskellStyle { P.reservedOpNames = ["*","/","+","-","^"] } )

expr :: Parsed st Formula
expr = buildExpressionParser operatorDefs funCall
    <?> "expression"

funCall :: Parsed st Formula
funCall = try (liftM2 App var argList)
       <|> term
       <?> "funCall"

argList :: Parsed st [Formula]
argList = parens $ sepBy expr $ char ','

var :: Parsed st Formula
var = Variable <$> identifier

term :: Parsed st Formula
term = try (parens expr)
   <|> var
   <|> try (CFloat <$> float)
   <|> CInteger . fromInteger <$> integer
   <?> "Term error"

binary :: String -> (a -> a -> a) -> Assoc -> Operator Char st a
binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc

prefix :: String -> (a -> a) -> Operator Char st a
prefix  name fun       = Prefix (do{ reservedOp name; return fun })

{-postfix :: String -> (a -> a) -> Operator Char st a-}
{-postfix name fun       = Postfix (do{ reservedOp name; return fun })-}

operatorDefs :: OperatorTable Char st Formula
operatorDefs = 
    [ [prefix "-" (UnOp OpNegate) ]
    , [binary "^" (BinOp OpPow) AssocLeft]
    , [binary "/" (BinOp OpDiv) AssocLeft, binary "*" (BinOp OpMul) AssocLeft]
    , [binary "+" (BinOp OpAdd) AssocLeft, binary "-" (BinOp OpSub) AssocLeft]
    ]

