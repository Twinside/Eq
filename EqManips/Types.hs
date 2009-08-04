module EqManips.Types( Formula( .. )
                     , BinOperator( .. )
                     , UnOperator( .. )
                     , Entity( .. )
                     , prioOfBinaryOperators
                     , prioOfUnaryOperators
                     , expr 
                     , unparse
                     , isFormulaLeaf
                     , unOpNames
                     ) where

import Control.Applicative( (<$>) )

import Data.Ratio
import Data.List( intersperse, mapAccumR )
import Data.Maybe( fromJust )

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language( haskellStyle )
import qualified Text.ParserCombinators.Parsec.Token as P

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

    -- | '='
    | OpEq
    deriving (Eq,Show,Read)

data UnOperator =
      OpNegate
    | OpAbs
    | OpSqrt

    | OpSin
    | OpSinh
    | OpASin
    | OpASinh

    | OpCos
    | OpCosh
    | OpACos
    | OpACosh

    | OpTan
    | OpTanh
    | OpATan
    | OpATanh

    | OpLn
    | OpLog

    | OpExp
    deriving (Eq, Show, Read)

data Entity =
      Pi
    | Nabla
    deriving (Eq, Show, Read)

-- | Main type manipulated by the software.
-- All relevant instances for numeric types
-- are provided for ease of use
data Formula =
      Variable String
    | NumEntity Entity
    | CInteger Int
    | CFloat Double
    -- | FunName arguments
    | App Formula [Formula]
    -- | LowBound highbound expression
    | Sum Formula Formula Formula
    -- | LowBound highbound expression
    | Product Formula Formula Formula

    -- | Derivate expression withVar
    | Derivate Formula Formula

    -- | lowBound highBound expression dx
    | Integrate Formula Formula Formula Formula

    -- | -1 for example
    | UnOp UnOperator Formula

    -- | f1 op f2
    | BinOp BinOperator Formula Formula

    -- | Width, Height, all formulas
    | Matrix Int Int [[Formula]]

    -- | Used for debug
    | Block Int Int Int
    deriving (Eq, Show, Read)

type Parsed a b = GenParser Char a b

binopDefs :: [(BinOperator, (Int,String))]
binopDefs =
	[ (OpEq, (4, "="))
	, (OpAdd, (3, "+"))
	, (OpSub, (3, "-"))
	, (OpMul, (2, "*"))
	, (OpDiv, (2, "/"))
	, (OpPow, (1, "^"))
    ]

-------------------------------------------
---- "Language" helpers
-------------------------------------------
isFormulaLeaf :: Formula -> Bool
isFormulaLeaf (Variable _) = True
isFormulaLeaf (CInteger _) = True
isFormulaLeaf (CFloat _) = True
isFormulaLeaf (NumEntity _) = True
isFormulaLeaf _ = False

prioOfBinaryOperators :: BinOperator -> Int
prioOfBinaryOperators = prio
    where prio OpEq = 4
          prio OpAdd = 3
          prio OpSub = 3
          prio OpMul = 2
          prio OpDiv = 2
          prio OpPow = 1

prioOfUnaryOperators :: UnOperator -> Int
prioOfUnaryOperators = p
    where p OpNegate = 0
          p OpExp = 1
          p _ = 10000
    
unOpNames :: [(UnOperator, String)]
unOpNames =
    [ (OpNegate, "-")
    , (OpAbs, "abs")
    , (OpSqrt, "sqrt")

    , (OpSin, "sin")
    , (OpASin, "asin")
    , (OpSinh, "sinh")
    , (OpASinh, "asinh")

    , (OpCos, "cos")
    , (OpACos, "acos")
    , (OpCosh, "cosh")
    , (OpACosh, "acosh")

    , (OpTan, "tan")
    , (OpATan, "atan")
    , (OpTanh, "tanh")
    , (OpATanh, "atanh")

    , (OpLn, "ln")
    , (OpLog, "log")

    , (OpExp, "exp")
    ]

argListToString :: [Formula] -> String
argListToString fl = concat $ intersperse "," textArgs
    where accum _ f = ((), deparse maxPrio False f)
          (_,textArgs) = mapAccumR accum () fl

maxPrio :: Int
maxPrio = 15


-----------------------------------------------------------
--          Unprint
-----------------------------------------------------------
unparse :: Formula -> String
unparse = deparse maxPrio False

deparse :: Int -> Bool -> Formula -> String
deparse _ _ (Variable s) = s
deparse _ _ (NumEntity e) = en e
    where en Pi = "pi"
          en Nabla = "nabla"
deparse _ _ (CInteger i) = show i
deparse _ _ (CFloat d) = show d
deparse _ _ (Block i i1 i2) =
    "block(" ++ show i ++ "," ++ show i1 ++ "," ++ show i2 ++ ")"
deparse _ _ (App f1 fl) =
    deparse maxPrio False f1 ++ "(" ++ argListToString fl ++ ")"
deparse _ _ (Sum i i1 i2) =
    "sum(" ++ deparse maxPrio False i ++
        "," ++ deparse maxPrio False i1 ++ 
        "," ++ deparse maxPrio False i2 ++ ")"
deparse _ _ (Product i i1 i2) =
    "product(" ++ deparse maxPrio False i ++ 
            "," ++ deparse maxPrio False i1 ++ 
            "," ++ deparse maxPrio False i2 ++ ")"
deparse _ _ (Derivate i i1) =
    "derivate(" ++ deparse maxPrio False i ++ 
            "," ++ deparse maxPrio False i1 ++ ")"

deparse _ _ (Integrate i i1 i2 i3) =
    "integrate(" ++ deparse maxPrio False i ++ 
             "," ++ deparse maxPrio False i1 ++
             "," ++ deparse maxPrio False i2 ++ 
             "," ++ deparse maxPrio False i3 ++ ")"
deparse _ _ (UnOp op f) =
    (fromJust $ lookup op unOpNames) ++ 
        "(" ++ deparse maxPrio False f ++ ")"

-- Special case... as OpEq is right associative...
-- we must reverse shit for serialisation
deparse oldPrio right (BinOp OpEq f1 f2) =
    let (prio, txt) = fromJust $ lookup OpEq binopDefs
    in
    if prio > oldPrio || (not right && prio == oldPrio)
       then "(" ++ deparse prio False f1 
                ++ ' ' : txt ++ " " 
                ++ deparse prio True f2 ++ ")"
       else deparse prio False f1 
            ++ ' ' : txt ++ " "
            ++ deparse prio True f2

deparse oldPrio right (BinOp op f1 f2) =
    let (prio, txt) = fromJust $ lookup op binopDefs
    in
    if prio > oldPrio || (right && prio == oldPrio)
       then "(" ++ deparse prio False f1 
                ++ ' ' : txt ++ " " 
                ++ deparse prio True f2 ++ ")"
       else deparse prio False f1 
            ++ ' ' : txt ++ " "
            ++ deparse prio True f2

deparse _ _ (Matrix n m fl) =
    "matrix(" ++ show n ++ "," ++ show m ++ "," ++
            (argListToString $ concat fl) ++ ")"

----------------------------------------
----  Strong and valid instances    ----
----------------------------------------
{-instance Show Formula where-}
    {-show = deparse maxPrio False-}
    
instance Num Formula where
    (+) = BinOp OpAdd
    (-) = BinOp OpSub
    (*) = BinOp OpMul
    negate = UnOp OpNegate
    abs = UnOp OpAbs
    signum (CInteger n) = CInteger (signum n)
    signum (CFloat f) = CFloat (signum f)
    signum _ = CInteger 0
    fromInteger = CInteger . fromInteger

instance Fractional Formula where
    (/) = BinOp OpDiv 
    recip = BinOp OpDiv (CInteger 1)
    fromRational a = BinOp OpDiv (int $ numerator a) 
                                 (int $ denominator a)
            where int = CInteger . fromInteger
    
instance Floating Formula where
    pi = CFloat pi 
    exp = UnOp OpExp
    sqrt = UnOp OpSqrt
    log = UnOp OpLn
    (**) = BinOp OpPow
    sin = UnOp OpSin
    cos = UnOp OpCos
    tan = UnOp OpTan
    asin = UnOp OpASin
    acos = UnOp OpACos
    atan = UnOp OpATan
    sinh = UnOp OpSinh
    cosh = UnOp OpCosh
    tanh = UnOp OpTanh
    asinh = UnOp OpASinh
    acosh = UnOp OpACosh
    atanh = UnOp OpATanh

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
         (haskellStyle { P.reservedOpNames = ["*","/","+","-","^","="] } )

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
    , [binary "=" (BinOp OpEq) AssocRight]
    ]

