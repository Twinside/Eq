{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
module EqManips.Types
         ( FormulaPrim( .. )
         , Formula( .. )
         , ListForm -- ^ Tell that the formula is in form
                    -- binop op [a,b ...]
         , TreeForm -- ^ Tell that the formula is in form
                    -- binop op [a,b]

         , BinOperator( .. )
         , UnOperator( .. )
         , Entity( .. )

         , program  -- if you want to define some definition before
         , expr     -- if you want to evaluate just an expression
         , unparse  -- regurgitation in parsed language.
         , unparseS -- regurgitation with type ShowS

         , binopString
         , unopString
         , AssocSide(..) -- To query associativity side
         , OpAssoc( .. ) -- Return type for associativity side
         , Priority(.. ) -- Gain access to operator's priority
         , LeafNode( .. )
         , OpProp( .. ) 
         , OperatorText(..)

         , MetaOperation( .. )
         , Polynome( .. )
         , foldf
         , canDistributeOver 
         , distributeOver 
         ) where

import Control.Applicative( (<$>), (<*) )
import Control.Monad.Identity
import Data.Monoid( Monoid( .. ), getSum )
import qualified Data.Monoid as Monoid

import Data.Ratio
import Data.List( foldl', foldl1' )
import Data.Maybe( fromJust )

import EqManips.Propreties

import Text.Parsec.Expr
import Text.Parsec
import Text.Parsec.Language( haskellStyle )
import qualified Text.Parsec.Token as P

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

    | OpAnd -- ^ '&'
    | OpOr -- ^ '|'

    | OpEq -- ^ '='
	| OpNe -- ^ '/='
	| OpLt -- ^ '<'
	| OpGt -- ^ '>'
	| OpGe -- ^ '>='
	| OpLe -- ^ '<='

    | OpAttrib -- ^ ':='
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
    | OpFactorial
    | OpCeil | OpFloor | OpFrac
    deriving (Eq, Show, Read)

-- | Some entity which cannot be represented in other mannear
data Entity =
      Pi
    | Nabla
    | Infinite
    deriving (Eq, Show, Read, Ord)

data MetaOperation =
    -- | Avoid an evaluation, replace itself by the
    -- without touching it.
      Hold
    -- | Inverse of hold, whenever encountered in
    -- evaluation, should force an evaluation.
    | Force
    | Listify   -- ^ Pack operation a list-like form.
    | Treefy    -- ^ Pack operation in a tree-like form
    | Expand    -- ^ trigger an expend operation
    | Cleanup   -- ^ trigger a basic formula cleanup
    | LambdaBuild -- ^ To generate a full blown Lambda
    | Sort      -- ^ To sort the formula
    deriving (Eq, Show, Read)

type FloatingValue = Double

-- | Main type manipulated by the software.
-- All relevant instances for numeric types
-- are provided for ease of use
data FormulaPrim =
      Variable String
    | NumEntity Entity
    | Truth Bool
    | CInteger Integer
    | CFloat FloatingValue
    -- | FunName arguments
    | App FormulaPrim [FormulaPrim]
    -- | LowBound highbound expression
    | Sum FormulaPrim FormulaPrim FormulaPrim
    -- | LowBound highbound expression
    | Product FormulaPrim FormulaPrim FormulaPrim

    -- | Derivate expression withVar
    | Derivate FormulaPrim FormulaPrim

    -- | lowBound highBound expression dx
    | Integrate FormulaPrim FormulaPrim FormulaPrim FormulaPrim

    -- | -1 for example
    | UnOp UnOperator FormulaPrim

    -- | Represent a function. a function
    -- can have many definitions. The applied
    -- one must be the first in the list which
    -- unify with the applied parameters.
    | Lambda [( [FormulaPrim] {- clause args -}
              , FormulaPrim {- clause body -})
             ] {- clauses -}

    -- | f1 op f2
    | BinOp BinOperator [FormulaPrim]

    -- | Width, Height, all formulas
    | Matrix Int Int [[FormulaPrim]]

    -- | Used for debug
    | Block Int Int Int

    -- | A meta operation is an operation used
    -- by the sysem, but that doesn't appear in the
    -- normal output.
    | Meta MetaOperation FormulaPrim
    deriving (Eq, Show, Read)

-- | Type used to carry some meta information
-- with the type system.
-- - formula Form : how is handled the binop form
newtype Formula formulaForm = Formula { unTagFormula :: FormulaPrim }
    deriving (Eq, Show, Ord)

-- | Type token for format of the form [a,b,c,d,e...]
data ListForm
-- | Type token for format of the form [a,b]
data TreeForm
-- | Ok the data doesn't have any specific form
data NoForm

-- | This type store polynome in a recursive way, as presented
-- in chapter 3 of "Algorithm for Computer Algebra". It's a
-- recursive linked list
data Polynome =
      Polynome String [(Int, Polynome)]
    | PolyCoeffF FloatingValue
    | PolyCoeffI Integer
    deriving (Eq, Show, Read)

{-data PowerSerie-}

infixl 4 <<>>

(<<>>) :: Ordering -> Ordering -> Ordering
a <<>> b = ordIt a
    where ordIt EQ = b
          ordIt o = o

-----------------------------------------------------------
--  Ord def, used to sort-out '+' list for exemples
-----------------------------------------------------------
instance Ord FormulaPrim where
    -- Ignoring meta in comparisons
    compare (Meta _ f) f2 = compare f f2
    compare f (Meta _ f2) = compare f f2

    compare (NumEntity e1) (NumEntity e2) = compare e1 e2
    compare (UnOp _ f1) (UnOp _ f2) = compare f1 f2

    compare (CInteger i) (CInteger i2) = compare i i2
    compare (CFloat f) (CFloat f2) = compare f f2
    compare (CInteger i) (CFloat f) = compare (fromIntegral i) f
    compare (CFloat f) (CInteger i) = compare f $ fromIntegral i
    compare (CFloat _) _ = LT
    compare (CInteger _) _ = LT

    -- x < y
    compare (Variable v) (Variable v1) = compare v v1
    -- Variable last
    compare (Variable _) _ = LT

    compare _ (CInteger _) = GT
    compare _ (CFloat _) = GT
    compare _ (Block _ _ _) = LT
    compare _ (NumEntity _) = GT

    -- we don't sort matrixes, because the mul
    compare (Matrix _ _ _) (Matrix _ _ _) = EQ
    compare _ (Matrix _ _ _) = LT
    compare (Matrix _ _ _) _ = LT

    compare (BinOp OpPow [Variable v1, p1])
            (BinOp OpPow [Variable v2, p2])
            | p1 == p2 = compare v1 v2
            | otherwise = compare p1 p2
    
    compare (BinOp OpPow _) _ = GT
    compare _ (BinOp OpPow _) = LT

    compare (BinOp op (BinOp OpPow (Variable v1: p1: _):_))
            (BinOp op' (BinOp OpPow (Variable v2: p2: _):_))
        | op == op' && v1 == v2 && op `elem` [OpMul, OpDiv] = compare p1 p2

    compare (BinOp op (_:(BinOp OpPow (Variable v1: p1: _):_)))
            (BinOp op' (_:(BinOp OpPow (Variable v2: p2: _):_)))
        | op == op' && v1 == v2 && op `elem` [OpMul, OpDiv] = compare p1 p2

    compare (BinOp _ f1) (BinOp _ f2) = compare f1 f2

    compare (Derivate w _) (Derivate w' _) = compare w w'
    compare (Derivate _ _) (Integrate _ _ _ _) = LT
    compare (Derivate _ _) _ = GT

    compare (Integrate _ _ w _) (Integrate _ _ w' _) = compare w w'
    compare (Integrate _ _ _ _) _ = GT
    compare (Product l h w) (Product l' h' w') =
        compare l l' <<>> compare h h' <<>> compare w w'
    compare (Product _ _ _) _ = GT

    compare (Sum l h w) (Sum l' h' w') =
        compare l l' <<>> compare h h' <<>> compare w w'
    compare (Sum _ _ _) _ = GT

    compare (App _ _) _ = LT

    compare (Block _ _ _) _ = GT
    compare (NumEntity _) _ = LT
    compare f1 f2 = compare (nodeCount f1) $ nodeCount f2
        where nodeCount = getSum . foldf 
                    (\_ a -> Monoid.Sum $ getSum a + 1)
                    (Monoid.Sum 0 :: Monoid.Sum Int)
    

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
-- transformation based on operators. Distributivity
-- is handled elsewhere because we need to specify which
-- operator we can distribute uppon.
data OpProp = Associativ -- ^ if (a . b) . c <=> a . (b . c)
    | Commutativ         -- ^ if a . b = b . a
    | Distributiv        -- ^ if a . (b ! c) <=> a . b ! a . c
                         -- /!\ must check on what it is distributiv
    | InverseOp          -- ^ Inverse operation
    deriving (Eq, Show)

emptyProps :: e -> [p] -> [(p,e)]
emptyProps e = map $ flip (,) e

instance Property BinOperator OpProp BinOperator where
    getProps OpEq  = []

    getProps OpAnd = []
    getProps OpOr = []
    getProps OpNe = []
    getProps OpLe = []
    getProps OpGe = []
    getProps OpLt = []
    getProps OpGt = []

    getProps OpPow = []
    getProps OpAttrib = []

    getProps OpSub = [(InverseOp, OpAdd)]
    getProps OpAdd =
        (InverseOp, OpSub) : emptyProps OpAdd [Associativ, Commutativ]
    getProps OpMul =
        (InverseOp, OpDiv) : emptyProps OpMul [Associativ, Commutativ, Distributiv]
    getProps OpDiv = 
        (InverseOp, OpMul) : emptyProps OpDiv [Distributiv]

canDistributeOver :: BinOperator -> BinOperator -> Bool
canDistributeOver op1 op2 = op2 `elem` distributeOver op1

distributeOver :: BinOperator -> [BinOperator]
distributeOver OpMul = [OpAdd, OpSub]
distributeOver OpDiv = [OpAdd, OpSub]
distributeOver OpOr = [OpAnd]
distributeOver _ = []

-----------------------------------------------------------
--          Priority Property
-----------------------------------------------------------
data Priority = Priority deriving Eq

instance Property BinOperator Priority Int where
    getProps op = [(Priority, fst . fromJust $ lookup op binopDefs)]
    
instance Property UnOperator Priority Int where
    getProps OpFactorial = [(Priority, 0)]
    getProps OpNegate = [(Priority, 1)]
    getProps OpExp = [(Priority, 2)]
    getProps _ = [(Priority, 1000)]

-----------------------------------------------------------
--          Leaf Property
-----------------------------------------------------------
data LeafNode = LeafNode deriving Eq

instance Property FormulaPrim LeafNode Bool where
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
    
-- | Priority and textual representation
-- of binary operators
binopDefs :: [(BinOperator, (Int,String))]
binopDefs =
	[ (OpAttrib, (7, ":="))
    , (OpAnd, (6, "&"))
    , (OpOr, (6, "|"))
    , (OpEq, (5, "="))
    , (OpNe, (5, "/="))
    , (OpLt, (5, "<"))
    , (OpGt, (5, ">"))
    , (OpGe, (5, ">="))
    , (OpLe, (5, "<="))
	, (OpAdd, (4, "+"))
	, (OpSub, (4, "-"))
	, (OpMul, (3, "*"))
	, (OpDiv, (3, "/"))
	, (OpPow, (2, "^"))
    ]

binopString :: BinOperator -> String
binopString a = snd . fromJust $ lookup a binopDefs

unopString :: UnOperator -> String
unopString a = fromJust $ lookup a unOpNames

-- | Textual representation of "unary" operators
unOpNames :: [(UnOperator, String)]
unOpNames =
    [ (OpNegate, "-")
    , (OpFactorial, "!")
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
    , (OpCeil, "ceil")
    , (OpFloor, "floor")
    , (OpFrac, "frac")
    ]
 
-------------------------------------------
---- Formula Folding
-------------------------------------------
foldf :: (Monoid b)
      => (FormulaPrim -> b -> b) -> b -> FormulaPrim -> b
foldf f acc m@(Meta _ fo) = f m $ foldf f acc fo
foldf f acc fo@(UnOp _ sub) = f fo $ foldf f acc sub
foldf f acc fo@(App def args) =
    f fo (foldf f listAcc def)
     where listAcc = foldr f acc args

foldf f acc fo@(BinOp _ args) =
    f fo $ foldr f acc args

foldf f acc fo@(Sum ini end what) = f fo finalAcc
    where whatAcc = foldf f acc what
          iniAcc = foldf f acc ini
          endAcc = foldf f acc end
          finalAcc = whatAcc `mappend` iniAcc `mappend` endAcc

foldf f acc fo@(Product ini end what) = f fo finalAcc
        where whatAcc = foldf f acc what
              iniAcc = foldf f acc ini
              endAcc = foldf f acc end
              finalAcc = whatAcc `mappend` iniAcc `mappend` endAcc

foldf f acc fo@(Integrate ini end what var) = f fo finalAcc
        where whatAcc = foldf f acc what
              iniAcc = foldf f acc ini
              endAcc = foldf f acc end
              varAcc = foldf f acc var
              finalAcc = whatAcc `mappend` iniAcc 
                                 `mappend` endAcc `mappend` varAcc

foldf f acc fo@(Derivate what var) = f fo $ whatAcc `mappend` varAcc
        where whatAcc = foldf f acc what
              varAcc = foldf f acc var

foldf f acc fo@(Matrix _ _ cells) = f fo finalAcc
    where lineFolder acc' formu = acc' `mappend` (foldf f acc formu)
          rowAccs = [ foldl' lineFolder mempty row | row <- cells]
          finalAcc = foldl1' mappend rowAccs

foldf f acc fo = f fo acc

-------------------------------------------
---- "Language" helpers
-------------------------------------------
    
-- | used to render functions' arguments
argListToString :: [FormulaPrim] -> ShowS
argListToString [] = id
argListToString [f] = deparse maxPrio False f
argListToString lst = foldl' accum (unprint lastElem) reved
    where unprint = deparse maxPrio False
          accum acc f = unprint f . (',':) . acc
          (lastElem:reved) = reverse lst

-- | only to avoid a weird constant somewhere
maxPrio :: Int
maxPrio = 15

-----------------------------------------------------------
--          Unprint
-----------------------------------------------------------

-- | Public function to translate a formula back to it's
-- original notation. NOTE : it's not used as a Show instance...
unparse :: FormulaPrim -> String
unparse f = unparseS f ""

unparseS :: FormulaPrim -> ShowS
unparseS  = deparse maxPrio False

-- | Real conversion function, pass down priority
-- and tree direction
deparse :: Int -> Bool -> FormulaPrim -> ShowS
-- INVISIBLE META NINJA !!
deparse i r (Meta op f) = (++) (show op) . ('(' :) . deparse i r f . (')':)
deparse _ _ (Truth True) = ("true" ++)
deparse _ _ (Truth False) = ("false" ++)
deparse _ _ (BinOp _ []) =
    error "The formula is denormalized : a binary operator without any operands"
deparse _ _ (Variable s) = (s ++)
deparse _ _ (Lambda _) = id -- NINJA HIDDEN!
deparse _ _ (NumEntity e) = (en e ++)
    where en Pi = "pi"
          en Nabla = "nabla"
          en Infinite = "infinite"
deparse _ _ (CInteger i) = shows i
deparse _ _ (CFloat d) = shows d
deparse _ _ (Block i i1 i2) =
    ("block(" ++) . shows i . (',':) . shows i1 . (',' :) . shows i2 . (')' :)

deparse _ _ (App (Variable v) fl) =
    (v ++) . ('(' :) . argListToString fl . (')' :)

deparse _ _ (App f1 fl) =
    ('(' :) . deparse maxPrio False f1 . (")(" ++) . argListToString fl . (')' :)

deparse _ _ (Sum i i1 i2) =
    ("sum(" ++) . argListToString [i, i1, i2] . (')':)

deparse _ _ (Product i i1 i2) =
    ("product(" ++) . argListToString [i, i1, i2] . (')':)

deparse _ _ (Derivate i i1) =
    ("derivate(" ++) . argListToString [i, i1] . (')':)

deparse _ _ (Integrate i i1 i2 i3) =
    ("integrate(" ++) . argListToString [i, i1, i2, i3] . (')':)

deparse _ _ (UnOp OpFactorial f) = ('(':) . deparse maxPrio False f . (")!" ++)
deparse _ _ (UnOp op f) =
    ((++) . fromJust $ lookup op unOpNames) . 
        ('(':) . deparse maxPrio False f . (')':)

-- Special case... as OpEq is right associative...
-- we must reverse shit for serialisation
deparse oldPrio right (BinOp OpEq [f1,f2]) =
    let (prio, txt) = fromJust $ lookup OpEq binopDefs
    in
    if prio > oldPrio || (not right && prio == oldPrio)
       then ('(':) 
                . deparse prio False f1 
                . (' ' :) . (txt ++) . (' ':) 
                . deparse prio True f2 . (')':)
       else deparse prio False f1 
            . (' ' :) . (txt ++) . (' ':)
            . deparse prio True f2

deparse oldPrio right (BinOp op [f1,f2]) =
    let (prio, txt) = fromJust $ lookup op binopDefs
    in
    if prio > oldPrio || (right && prio == oldPrio)
       then ('(':) . deparse prio False f1 
                . (' ' :) . (txt ++) . (' ':) 
                . deparse prio True f2 . (')':)
       else deparse prio False f1 
            . (' ' :) . (txt ++) . (' ':)
            . deparse prio True f2

deparse oldPrio right (BinOp op (f1:xs)) =
    let (prio, txt) = fromJust $ lookup op binopDefs
    in
    if prio > oldPrio || (right && prio == oldPrio)
       then ('(':) . deparse prio False f1 
                . (' ':) . (txt ++) . (' ':) 
                . deparse prio False (BinOp op xs) . (')':)
       else deparse prio False f1 
            . (' ' :) . (txt ++) . (' ':)
            . deparse prio False (BinOp op xs)

deparse _ _ (Matrix n m fl) =
    ("matrix("++) . shows n 
                  . (',':) 
                  . shows m 
                  . (',':) .  (argListToString $ concat fl) . (')':)

----------------------------------------
----  Strong and valid instances    ----
----------------------------------------
instance Num FormulaPrim where
    a + b = BinOp OpAdd [a,b]
    a - b = BinOp OpSub [a,b]
    a * b = BinOp OpMul [a,b]
    negate = UnOp OpNegate
    abs = UnOp OpAbs
    signum (CInteger n) = CInteger (signum n)
    signum (CFloat f) = CFloat (signum f)
    signum _ = CInteger 0
    fromInteger = CInteger . fromInteger

instance Fractional FormulaPrim where
    a / b = BinOp OpDiv [a,b]
    recip b = BinOp OpDiv [CInteger 1, b]
    fromRational a = BinOp OpDiv [ int $ numerator a
                                 , int $ denominator a]
            where int = CInteger . fromInteger
    
instance Floating FormulaPrim where
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
    [ [postfix "!" (UnOp OpFactorial)]
    , [prefix "-" (UnOp OpNegate) ]
    , [binary "^" (binop OpPow) AssocLeft]
    , [binary "/" (binop OpDiv) AssocLeft, binary "*" (binop OpMul) AssocLeft]
    , [binary "+" (binop OpAdd) AssocLeft, binary "-" (binop OpSub) AssocLeft]
    , [binary "=" (binop OpEq)  AssocRight, binary "/=" (binop OpNe) AssocLeft
      ,binary "<" (binop OpLt)  AssocLeft,  binary ">"  (binop OpGt) AssocLeft
      ,binary "<=" (binop OpLe) AssocLeft,  binary ">=" (binop OpGe) AssocLeft]
    , [binary "&" (binop OpAnd) AssocLeft, binary "|" (binop OpOr) AssocLeft]
    , [binary ":=" (binop OpAttrib) AssocRight]
    ]

funCall :: Parsed st FormulaPrim
funCall =  do
    caller <- term
    (App caller <$> argList) <|> return caller
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
trueConst = (return $ Truth True) <* (string "true" >> whiteSpace)

falseConst :: Parsed st FormulaPrim
falseConst = (return $ Truth False) <* (string "false" >> whiteSpace)
-----------------------------------------------
----        Little helpers
-----------------------------------------------
binary :: String -> (a -> a -> a) -> Assoc -> Operator String st Identity a
binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc

prefix :: String -> (a -> a) -> Operator String st Identity a
prefix  name fun       = Prefix (do{ reservedOp name; return fun })

postfix :: String -> (a -> a) -> Operator String st Identity a
postfix name fun = Postfix (do{ reservedOp name; return fun })

binop :: BinOperator -> FormulaPrim -> FormulaPrim -> FormulaPrim
binop op left right = BinOp op [left, right]

