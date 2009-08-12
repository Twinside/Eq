{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module EqManips.Types( Formula( .. )
                     , BinOperator( .. )
                     , UnOperator( .. )
                     , Entity( .. )

                     , program  -- if you want to define some definition before
                     , expr     -- if you want to evaluate just an expression
                     , unparse  -- regurgitation in parsed language.

                     , AssocSide(..) -- To query associativity side
                     , OpAssoc( .. ) -- Return type for associativity side
                     , Priority(.. ) -- Gain access to operator's priority
                     , LeafNode( .. )
                     , OpProp( .. ) 
                     , OperatorText(..)
                     ) where

import Control.Applicative( (<$>) )

import Data.Ratio
import Data.List( intersperse, mapAccumR )
import Data.Maybe( fromJust )

import EqManips.Propreties

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

-- | All `unary` operators are in there. some are mathematical
-- functions. They're present here, because it's easier to pattern
-- match them this way
data UnOperator =
      OpNegate | OpAbs | OpSqrt

    | OpSin | OpSinh | OpASin | OpASinh
    | OpCos | OpCosh | OpACos | OpACosh
    | OpTan | OpTanh | OpATan | OpATanh

    | OpLn | OpLog | OpExp
    deriving (Eq, Show, Read)

-- | Some entity which cannot be represented in other mannear
data Entity =
      Pi
    | Nabla
    | Infinite
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

    -- | Represent a function. a function
    -- can have many definitions. The applied
    -- one must be the first in the list which
    -- unify with the applied parameters.
    | Lambda [([Formula], Formula)]

    -- | f1 op f2
    | BinOp BinOperator [Formula]

    -- | Width, Height, all formulas
    | Matrix Int Int [[Formula]]

    -- | Used for debug
    | Block Int Int Int
    deriving (Eq, Show, Read)

-----------------------------------------------------------
--          Side Associativity
-----------------------------------------------------------
-- | Used to retrieve association property of operators.
-- It's only a type token
data AssocSide = AssocSide
    deriving (Eq)

-- | The implementation of property operators
data OpAssoc = OpAssocLeft | OpAssocRight
    deriving (Eq, Show)

-- | Help to query operator associativity
instance Property BinOperator AssocSide OpAssoc where
    getProps OpEq = [(AssocSide, OpAssocRight)] 
    getProps _  = [(AssocSide, OpAssocLeft)]

-----------------------------------------------------------
--          General operator property
-----------------------------------------------------------
-- | Some use full informations which can be used for$
-- transformation based on operators
data OpProp = Associativ -- ^ if (a . b) . c <=> a . (b . c)
    | Distributiv        -- ^ if a . (b ! c) <=> a . b ! a . c
    | Commutativ         -- ^ if a . b = b . a
    | InverseOp          -- ^ Inverse operation
    deriving (Eq, Show)

emptyProps :: e -> [p] -> [(p,e)]
emptyProps e = map $ flip (,) e

instance Property BinOperator OpProp BinOperator where
    getProps OpEq  = []
    getProps OpPow = []
    getProps OpSub = [(InverseOp, OpAdd)]
    getProps OpAdd =
        (InverseOp, OpSub) : emptyProps OpAdd [Associativ, Commutativ]
    getProps OpMul =
        (InverseOp, OpDiv) : emptyProps OpMul [Associativ, Commutativ, Distributiv]
    getProps OpDiv = 
        (InverseOp, OpMul) : emptyProps OpDiv [Distributiv]

-----------------------------------------------------------
--          Priority Property
-----------------------------------------------------------
data Priority = Priority deriving Eq

instance Property BinOperator Priority Int where
    getProps op = [(Priority, fst . fromJust $ lookup op binopDefs)]
    
instance Property UnOperator Priority Int where
    getProps OpNegate = [(Priority, 0)]
    getProps OpExp = [(Priority, 1)]
    getProps _ = [(Priority, 1000)]

-----------------------------------------------------------
--          Leaf Property
-----------------------------------------------------------
data LeafNode = LeafNode deriving Eq

instance Property Formula LeafNode Bool where
    getProps (Variable _) = [(LeafNode, True)]
    getProps (CInteger _) = [(LeafNode, True)]
    getProps (CFloat _) = [(LeafNode, True)]
    getProps (NumEntity _) = [(LeafNode, True)]
    getProps _ = [(LeafNode, False)]

    hasProp (Variable _) _ = True
    hasProp (CInteger _) _ = True
    hasProp (CFloat _) _ = True
    hasProp (NumEntity _) _ = True
    hasProp _ _ = False

-----------------------------------------------------------
--          Text
-----------------------------------------------------------
data OperatorText = OperatorText deriving Eq

instance Property UnOperator OperatorText String where
    getProps op = [(OperatorText, fromJust $ lookup op unOpNames)]
    
type Parsed a b = GenParser Char a b

-- | Priority and textual representation
-- of binary operators
binopDefs :: [(BinOperator, (Int,String))]
binopDefs =
	[ (OpEq, (4, "="))
	, (OpAdd, (3, "+"))
	, (OpSub, (3, "-"))
	, (OpMul, (2, "*"))
	, (OpDiv, (2, "/"))
	, (OpPow, (1, "^"))
    ]

-- | Textual representation of "unary" operators
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

-------------------------------------------
---- "Language" helpers
-------------------------------------------
    
-- | used to render functions' arguments
argListToString :: [Formula] -> String
argListToString fl = concat $ intersperse "," textArgs
    where accum _ f = ((), deparse maxPrio False f)
          (_,textArgs) = mapAccumR accum () fl

-- | only to avoid a weird constant somewhere
maxPrio :: Int
maxPrio = 15

-----------------------------------------------------------
--          Unprint
-----------------------------------------------------------

-- | Public function to translate a formula back to it's
-- original notation. NOTE : it's not used as a Show instance...
unparse :: Formula -> String
unparse = deparse maxPrio False

-- | Real conversion function, pass down priority
-- and tree direction
deparse :: Int -> Bool -> Formula -> String
deparse _ _ (BinOp _ []) =
    error "The formula is denormalized : a binary operator without any operands"
deparse _ _ (Variable s) = s
deparse _ _ (Lambda _) = "" -- NINJA HIDDEN!
deparse _ _ (NumEntity e) = en e
    where en Pi = "pi"
          en Nabla = "nabla"
          en Infinite = "infinite"
deparse _ _ (CInteger i) = show i
deparse _ _ (CFloat d) = show d
deparse _ _ (Block i i1 i2) =
    "block(" ++ show i ++ "," ++ show i1 ++ "," ++ show i2 ++ ")"

deparse _ _ (App f1 fl) =
    deparse maxPrio False f1 ++ "(" ++ argListToString fl ++ ")"

deparse _ _ (Sum i i1 i2) =
    "sum(" ++ argListToString [i, i1, i2]  ++ ")"

deparse _ _ (Product i i1 i2) =
    "product(" ++ argListToString [i, i1, i2]  ++ ")"

deparse _ _ (Derivate i i1) =
    "derivate(" ++ argListToString [i, i1] ++ ")"

deparse _ _ (Integrate i i1 i2 i3) =
    "integrate(" ++ argListToString [i, i1, i2, i3] ++ ")"

deparse _ _ (UnOp op f) =
    (fromJust $ lookup op unOpNames) ++ 
        "(" ++ deparse maxPrio False f ++ ")"

-- Special case... as OpEq is right associative...
-- we must reverse shit for serialisation
deparse oldPrio right (BinOp OpEq [f1,f2]) =
    let (prio, txt) = fromJust $ lookup OpEq binopDefs
    in
    if prio > oldPrio || (not right && prio == oldPrio)
       then "(" ++ deparse prio False f1 
                ++ ' ' : txt ++ " " 
                ++ deparse prio True f2 ++ ")"
       else deparse prio False f1 
            ++ ' ' : txt ++ " "
            ++ deparse prio True f2

deparse oldPrio right (BinOp op [f1,f2]) =
    let (prio, txt) = fromJust $ lookup op binopDefs
    in
    if prio > oldPrio || (right && prio == oldPrio)
       then "(" ++ deparse prio False f1 
                ++ ' ' : txt ++ " " 
                ++ deparse prio True f2 ++ ")"
       else deparse prio False f1 
            ++ ' ' : txt ++ " "
            ++ deparse prio True f2

deparse oldPrio right (BinOp op (f1:xs)) =
    let (prio, txt) = fromJust $ lookup op binopDefs
    in
    if prio > oldPrio || (right && prio == oldPrio)
       then "(" ++ deparse prio False f1 
                ++ ' ' : txt ++ " " 
                ++ deparse prio False (BinOp op xs) ++ ")"
       else deparse prio False f1 
            ++ ' ' : txt ++ " "
            ++ deparse prio False (BinOp op xs)

deparse _ _ (Matrix n m fl) =
    "matrix(" ++ show n ++ "," ++ show m ++ "," ++
            (argListToString $ concat fl) ++ ")"

----------------------------------------
----  Strong and valid instances    ----
----------------------------------------
instance Num Formula where
    a + b = BinOp OpAdd [a,b]
    a - b = BinOp OpSub [a,b]
    a * b = BinOp OpMul [a,b]
    negate = UnOp OpNegate
    abs = UnOp OpAbs
    signum (CInteger n) = CInteger (signum n)
    signum (CFloat f) = CFloat (signum f)
    signum _ = CInteger 0
    fromInteger = CInteger . fromInteger

instance Fractional Formula where
    a / b = BinOp OpDiv [a,b]
    recip b = BinOp OpDiv [CInteger 1, b]
    fromRational a = BinOp OpDiv [ int $ numerator a
                                 , int $ denominator a]
            where int = CInteger . fromInteger
    
instance Floating Formula where
    pi = CFloat pi 
    exp = UnOp OpExp
    sqrt = UnOp OpSqrt
    log = UnOp OpLn
    a ** b = BinOp OpPow [a,b]
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
program :: Parsed st [Formula]
program = sepBy expr $ (char ';' >> whiteSpace)

-- | Parser for the mini language is defined here
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

binop :: BinOperator -> Formula -> Formula -> Formula
binop op left right = BinOp op [left, right]

operatorDefs :: OperatorTable Char st Formula
operatorDefs = 
    [ [prefix "-" (UnOp OpNegate) ]
    , [binary "^" (binop OpPow) AssocLeft]
    , [binary "/" (binop OpDiv) AssocLeft, binary "*" (binop OpMul) AssocLeft]
    , [binary "+" (binop OpAdd) AssocLeft, binary "-" (binop OpSub) AssocLeft]
    , [binary "=" (binop OpEq) AssocRight]
    ]

