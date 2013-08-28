{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Eq.Types
         ( FormulaPrim( .. )
         , Formula( .. )

         -- | Tell that the formula is in form binop op [a,b ...]
         , ListForm
         -- | Tell that formula is in form Binop op [a,b]
         , TreeForm

         , hashOfFormula   
         , BinOperator( .. )
         , UnOperator( .. )
         , Entity( .. )

         , binopString
         , unopString

         -- | Exported only to permit the main program to display
         -- accurate help.
         , binopDefs 
         -- | For more information about others unary operator,
         -- refer to the link section.
         , realUnopOperators

         -- | To query associativity side
         , AssocSide(..) 
         -- | Return type for associativity side
         , OpAssoc( .. ) 
         -- | Gain access to operator's priority
         , Priority(.. )
         , LeafNode( .. )
         , OpProp( .. ) 
         , OperatorText(..)

         , MetaOperation( .. )
         , Polynome( .. ), PolyCoeff( .. )
         , coeffPredicate, polyCoeffCast 
         , foldf
         , canDistributeOver 
         , distributeOver 

         , binOp, unOp, complex, meta
         , app, summ, productt, derivate
         , integrate, lambda, matrix, poly
         , indexes, list, infer, display, stack
         ) where

import Data.Data
import Data.Ord( comparing )
import Data.Monoid( Monoid( .. ), getSum )
import qualified Data.Monoid as Monoid
import qualified Language.Eq.ErrorMessages as Err

import Data.Bits
import Data.Ratio
import Data.List( foldl', foldl1' )
import Data.Maybe( fromJust )

import Language.Eq.Propreties
import {-# SOURCE #-} Language.Eq.Polynome()
import {-# SOURCE #-} Language.Eq.Renderer.Sexpr

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

    | OpLazyAttrib  -- ^ ':>'
    | OpAttrib      -- ^ ':='
    | OpCons        -- ^ '::'

    | OpType        -- ^ ':'
    | OpEntail      -- ^ entail(a,b)
    deriving (Eq,Show,Enum)

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
    
    | OpMatrixWidth | OpMatrixHeight
    deriving (Eq, Show, Enum)

-- | Some entity which cannot be represented in other mannear
data Entity =
      Pi
    | Nabla
    | Infinite
    | Ellipsis  -- ^ ... no value can be bound to it
    deriving (Eq, Show, Ord, Enum)


data MetaOperation =
    -- | Avoid an evaluation, replace itself by the
    -- without touching it.
      Hold
    -- | Inverse of hold, whenever encountered in
    -- evaluation, should force an evaluation.
    | Force
    | Expand    -- ^ trigger an expend operation
    | Cleanup   -- ^ trigger a basic formula cleanup
    | LambdaBuild -- ^ To generate a full blown Lambda
    | Sort      -- ^ To sort the formula
    deriving (Eq, Show, Read, Enum)

type FloatingValue = Double
type HashResume = Int

-- | Main type manipulated by the software.
-- All relevant instances for numeric types
-- are provided for ease of use
data FormulaPrim =
      Variable String
    | NumEntity Entity
    | Truth Bool
    | CInteger Integer
    | CFloat FloatingValue
    | Fraction (Ratio Integer)
    | Complex HashResume (FormulaPrim , FormulaPrim)

    -- | To index nDimensional data
    | Indexes HashResume FormulaPrim [FormulaPrim]
    -- | Yay, adding list to the language
    | List HashResume [FormulaPrim]

    | Display HashResume [FormulaPrim]
    | Stack HashResume [FormulaPrim]

    -- | FunName arguments
    | App HashResume FormulaPrim [FormulaPrim]
    -- | LowBound highbound expression
    | Sum HashResume FormulaPrim FormulaPrim FormulaPrim
    -- | LowBound highbound expression
    | Product HashResume FormulaPrim FormulaPrim FormulaPrim

    -- | Derivate expression withVar
    | Derivate HashResume FormulaPrim FormulaPrim

    -- | lowBound highBound expression dx
    | Integrate HashResume FormulaPrim FormulaPrim FormulaPrim FormulaPrim

    -- | -1 for example
    | UnOp HashResume UnOperator FormulaPrim

    -- | Represent a function. a function
    -- can have many definitions. The applied
    -- one must be the first in the list which
    -- unify with the applied parameters.
    | Lambda HashResume [( [FormulaPrim] {- clause args -}
                         , FormulaPrim {- clause body -})
                        ] {- clauses -}

    -- | f1 op f2
    | BinOp HashResume BinOperator [FormulaPrim]

    -- | Width, Height, all formulas
    | Matrix HashResume Int Int [[FormulaPrim]]

    -- | Inference rule
    | Infer HashResume [[FormulaPrim]] [FormulaPrim]

    -- | Form that can be used to make nice simplification.
    | Poly HashResume Polynome

    -- | Used for debug
    | Block Int Int Int

    -- | A meta operation is an operation used
    -- by the sysem, but that doesn't appear in the
    -- normal output.
    | Meta HashResume MetaOperation FormulaPrim
    deriving (Eq, Show)

--------------------------------------------------
----            Hash construction
--------------------------------------------------
hashOfFormula :: FormulaPrim -> HashResume
hashOfFormula (CInteger i) = fromIntegral i
hashOfFormula (Variable s) = sum $ map fromEnum s
hashOfFormula (NumEntity e) = fromEnum e
hashOfFormula (Truth True) = maxBound
hashOfFormula (Truth False) = minBound
hashOfFormula (CFloat f) = fromEnum f
hashOfFormula (Fraction frac) = fromIntegral (numerator frac)
                              + fromIntegral (denominator frac)

hashOfFormula (Complex hash _) = hash
hashOfFormula (Indexes hash _ _) = hash
hashOfFormula (List hash _) = hash
hashOfFormula (Display hash _) = hash
hashOfFormula (Stack hash _) = hash
hashOfFormula (App hash _ _) = hash
hashOfFormula (Sum hash _ _ _) = hash
hashOfFormula (Product hash _ _ _) = hash
hashOfFormula (Derivate hash _ _) = hash
hashOfFormula (Integrate hash _ _ _ _) = hash
hashOfFormula (UnOp hash _ _) = hash
hashOfFormula (Lambda hash _) = hash
hashOfFormula (BinOp hash _ _) = hash
hashOfFormula (Matrix hash _ _ _) = hash
hashOfFormula (Poly hash _) = hash
hashOfFormula (Block _ _ _) = 0
hashOfFormula (Meta hash _ _) = hash
hashOfFormula (Infer hash _ _) = hash

listHasher :: [FormulaPrim] -> HashResume
listHasher = foldl' hasher 0
    where hasher acc formula =
              (acc `rotateL` 3) `xor` hashOfFormula formula

nestedListHasher :: [[FormulaPrim]] -> HashResume
nestedListHasher = foldl' hasher 0
    where hasher acc formula =
              (acc `rotateL` 3) `xor` listHasher formula


polyCoeffHash :: PolyCoeff -> HashResume
polyCoeffHash (CoeffFloat f) = truncate $ 1000 * f
polyCoeffHash (CoeffInt i) = fromInteger i
polyCoeffHash (CoeffRatio r) = 100 * (fromInteger $ numerator r)
                             + (fromInteger $ denominator r)

polynomeHash :: Polynome -> HashResume
polynomeHash (PolyRest p) = polyCoeffHash p
polynomeHash (Polynome var coeffList) = varHash + coeffHash
    where varHash = sum $ map fromEnum var
          hasher acc (coeff, subPoly) =
              (acc `rotateR` 2) `xor` ( polyCoeffHash coeff
                                      + polynomeHash subPoly )
          coeffHash = foldl' hasher 0 coeffList

app :: FormulaPrim -> [FormulaPrim] -> FormulaPrim 
app what lst = App hash what lst
    where hash = (1 `shiftL` 3) `xor` (wHash `rotateL` 4) `xor` hashLst
          wHash = hashOfFormula what
          hashLst = listHasher lst

summ :: FormulaPrim -> FormulaPrim -> FormulaPrim -> FormulaPrim
summ a b c = Sum hash a b c
    where hash = (0xFF `shiftL` 15) + listHasher [a, b, c]

productt :: FormulaPrim -> FormulaPrim -> FormulaPrim -> FormulaPrim
productt a b c = Product hash a b c
    where hash = (0xFF `shiftL` 25) + listHasher [a, b, c]

derivate :: FormulaPrim -> FormulaPrim -> FormulaPrim
derivate what v = Derivate hash what v
    where hash = (0xCA03 `shiftL` 10) + (hashWhat `rotateL` 16) + hashVar
          hashWhat = hashOfFormula what
          hashVar = hashOfFormula v

integrate :: FormulaPrim -> FormulaPrim -> FormulaPrim -> FormulaPrim -> FormulaPrim 
integrate beg end what var = Integrate hash beg end what var
    where hash = 0xF00000F00 + hashSub
          hashSub = listHasher [beg, end, what, var]

lambda :: [([FormulaPrim], FormulaPrim)] -> FormulaPrim
lambda clauses = Lambda hash clauses
    where hash = xor 14
               $ foldr (\x acc -> (acc `rotateL` 2) + x) 0
                       [listHasher subs + hashOfFormula ap | (subs, ap) <- clauses]

matrix :: Int -> Int -> [[FormulaPrim]] -> FormulaPrim
matrix n m mlines = Matrix hash n m mlines
    where hash = ((n * m) `shiftL` 4) + 0xFF + subHash
          subHash = sum $ map listHasher mlines

poly :: Polynome -> FormulaPrim
poly createdPoly = Poly (polynomeHash createdPoly) createdPoly

binOp :: BinOperator -> [FormulaPrim] -> FormulaPrim
binOp op lst = BinOp hash op lst
    where hash = (4 `xor` (hashOp `shiftL` 2)) + listHasher lst
          hashOp = fromEnum op

unOp :: UnOperator -> FormulaPrim -> FormulaPrim
unOp op sub = UnOp hash op sub
    where hash = (5 `xor` (hashOp `shiftL` 4)) + subHash
          subHash = hashOfFormula sub
          hashOp = fromEnum op

complex :: (FormulaPrim, FormulaPrim) -> FormulaPrim
complex (re, im) = Complex hash (re, im)
    where hash = 7 + reHash + imHash `rotateR` 4
          reHash = hashOfFormula re
          imHash = hashOfFormula im

meta :: MetaOperation -> FormulaPrim -> FormulaPrim
meta op sub = Meta hash op sub
    where hash = (6 `xor` (opHash `shiftL` 8)) + (subHash `rotateR` 4)
          subHash = hashOfFormula sub
          opHash = fromEnum op

indexes :: FormulaPrim -> [FormulaPrim] -> FormulaPrim
indexes (Indexes _initHash a b) lst = Indexes hash a $ b ++ lst
    where hash = 0xAAAAAA `xor` (listHasher $ b ++ lst)

indexes a b = Indexes hash a b
    where hash = 0xAAAAAA `xor` (listHasher b)

list :: [FormulaPrim] -> FormulaPrim
list lst = List hash lst
    where hash = 0xBBBBBB `xor` listHasher lst

stack :: [FormulaPrim] -> FormulaPrim
stack lst = Stack hash lst
    where hash = 0xBCBBCB `xor` listHasher lst

display :: [FormulaPrim] -> FormulaPrim
display lst = Display hash lst
    where hash = 0xBCBBBB `xor` listHasher lst

infer :: [[FormulaPrim]] -> [FormulaPrim] -> FormulaPrim
infer l1 l2 = Infer hash l1 l2
    where hash = 0xBBBBDB `xor` nestedListHasher l1 `xor` listHasher l2

-- | Special binOp declaration used to merge two previous binary
-- operators. Update the hash rather than perform full recalculation.
binOpMerger :: BinOperator -> FormulaPrim -> FormulaPrim -> FormulaPrim
binOpMerger op (BinOp _ op1 lst1) (BinOp _ op2 lst2)
    | op == op1 && op == op2 = binOp op $ lst1 ++ lst2
binOpMerger op (BinOp _ op1 lst1) node2
    | op == op1 = binOp op $ lst1 ++ [node2]
binOpMerger op node1 (BinOp _ op2 lst2)
    | op == op2 = binOp op $ node1 : lst2
binOpMerger op node1 node2 = binOp op [node1, node2]

-- | Type used to carry some meta information
-- with the type system.
-- - formula Form : how is handled the binop form
newtype Formula formulaForm = Formula { unTagFormula :: FormulaPrim }
    deriving (Eq, {-Show,-} Ord, Typeable)

-- | Type token for format of the form [a,b,c,d,e...]
data ListForm
-- | Type token for format of the form [a,b]
data TreeForm
-- | Ok the data doesn't have any specific form

-- | Coefficient for polynoms
data PolyCoeff =
      CoeffFloat FloatingValue
    | CoeffInt Integer
    | CoeffRatio (Ratio Integer)
    deriving (Show, Read)

-- | This type store polynome in a recursive way, as presented
-- in chapter 3 of "Algorithm for Computer Algebra". It's a
-- recursive linked list
data Polynome =
      Polynome String [(PolyCoeff, Polynome)]
    | PolyRest PolyCoeff
    deriving (Eq, Show, Read)

instance Eq PolyCoeff where
    (==) = coeffPredicate (==)

coeffPredicate :: (forall a. Ord a => a -> a -> Bool) -> PolyCoeff -> PolyCoeff -> Bool
coeffPredicate op c1 c2 = eval $ polyCoeffCast c1 c2
    where eval (CoeffInt i1, CoeffInt i2) = i1 `op` i2
          eval (CoeffFloat f1, CoeffFloat f2) = f1 `op` f2
          eval (CoeffRatio r1, CoeffRatio r2) = r1 `op` r2
          eval _ = error Err.polynom_bad_casting 

-- | polyCoeffCast autocast to the same level
polyCoeffCast :: PolyCoeff -> PolyCoeff -> (PolyCoeff, PolyCoeff)
polyCoeffCast (CoeffInt i1) (CoeffInt i2) = (CoeffInt i1, CoeffInt i2)
polyCoeffCast (CoeffFloat f1) (CoeffFloat f2) = (CoeffFloat f1,CoeffFloat f2)
polyCoeffCast (CoeffRatio r1) (CoeffRatio r2) = (CoeffRatio r1, CoeffRatio r2)
polyCoeffCast (CoeffInt i1) (CoeffRatio r2) = (CoeffRatio $ i1 % 1, CoeffRatio r2)
polyCoeffCast (CoeffRatio r1) (CoeffInt i2) = (CoeffRatio r1, CoeffRatio $ i2 % 1)
polyCoeffCast (CoeffInt i1) (CoeffFloat f2) = (CoeffFloat $ fromInteger i1, CoeffFloat f2)
polyCoeffCast (CoeffFloat f1) (CoeffInt i2) = (CoeffFloat f1, CoeffFloat $ fromInteger i2)
polyCoeffCast (CoeffFloat f1) (CoeffRatio r2) = (CoeffFloat f1, CoeffFloat $ fromRational r2)
polyCoeffCast (CoeffRatio r1) (CoeffFloat f2) = (CoeffFloat $ fromRational r1, CoeffFloat f2)

infixl 4 <<>>

(<<>>) :: Ordering -> Ordering -> Ordering
a <<>> b = ordIt a
    where ordIt EQ = b
          ordIt o = o

-----------------------------------------------------------
--  Ord def, used to sort-out '+' list for exemples
-----------------------------------------------------------
instance Show (Formula anyForm) where
    showsPrec _ (Formula a) =
          ("{-"++)
        . sexprRenderS (Formula a)
        . (++) "-} Formula ("
        . shows a . (++) ")"

instance Ord PolyCoeff where
    compare left right = case polyCoeffCast left right of
        (CoeffInt a, CoeffInt b) -> compare a b
        (CoeffFloat a, CoeffFloat b) -> compare a b
        (CoeffRatio a, CoeffRatio b) -> compare a b
        _ -> error "Bad cast"

instance Ord Polynome where
    compare (PolyRest a) (PolyRest b) = compare a b
    compare (Polynome v1 c1) (Polynome v2 c2)
        | v1 /= v2 = compare v1 v2
        | otherwise = case compare coeff1 coeff2 of
                        EQ -> compare sub1 sub2
                        a -> a
            where (coeff1, sub1) = last c1
                  (coeff2, sub2) = last c2
    compare (Polynome _ _) _ = LT
    compare _ (Polynome _ _) = GT

instance Ord FormulaPrim where
    -- Ignoring meta in comparisons
    compare (Meta _ _ f) f2 = compare f f2
    compare f (Meta _ _ f2) = compare f f2

    compare (NumEntity e1) (NumEntity e2) = compare e1 e2
    compare (UnOp _ _ f1) (UnOp _ _ f2) = compare f1 f2

    compare (CInteger i) (CInteger i2) = compare i i2
    compare (CFloat f) (CFloat f2) = compare f f2
    compare (CInteger i) (CFloat f) = compare (fromIntegral i) f
    compare (CFloat f) (CInteger i) = compare f $ fromIntegral i
    compare (CFloat _) _ = LT
    compare (CInteger _) _ = LT

    compare (Poly _ p1) (Poly _ p2) = compare p1 p2
    compare (Poly _ _) _ = LT
    compare _ (Poly _ _) = GT

    -- x < y
    compare (Variable v) (Variable v1) = compare v v1
    -- Variable last
    compare (Variable _) _ = LT

    compare _ (CInteger _) = GT
    compare _ (CFloat _) = GT
    compare _ (Block _ _ _) = LT
    compare _ (NumEntity _) = LT

    -- we don't sort matrixes, because the mul
    compare (Matrix _ _ _ _) (Matrix _ _ _ _) = EQ
    compare _ (Matrix _ _ _ _) = LT
    compare (Matrix _ _ _ _) _ = LT

    compare (BinOp _ OpPow [Variable v1, p1])
            (BinOp _ OpPow [Variable v2, p2])
            | p1 == p2 = compare v1 v2
            | otherwise = compare p1 p2
    
    compare (BinOp _ OpPow a) (BinOp _ OpPow b) =
        case comparing length a b of
             LT -> LT
             EQ -> foldl' (\acc (a', b') -> acc <<>> compare a' b') EQ $ zip a b
             GT -> GT

    compare (BinOp _ OpPow _) _ = GT
    compare _ (BinOp _ OpPow _) = LT

    compare (BinOp _ op (BinOp _ OpPow (Variable v1: p1: _):_))
            (BinOp _ op' (BinOp _ OpPow (Variable v2: p2: _):_))
        | op == op' && v1 == v2 && op `elem` [OpMul, OpDiv] = compare p1 p2

    compare (BinOp _ op (_:(BinOp _ OpPow (Variable v1: p1: _):_)))
            (BinOp _ op' (_:(BinOp _ OpPow (Variable v2: p2: _):_)))
        | op == op' && v1 == v2 && op `elem` [OpMul, OpDiv] = compare p1 p2

    compare (BinOp _ _ f1) (BinOp _ _ f2) = compare f1 f2

    compare (Derivate _ w _) (Derivate _ w' _) = compare w w'
    compare (Derivate _ _ _) (Integrate _ _ _ _ _) = LT
    compare (Derivate _ _ _) _ = GT

    compare (Integrate _ _ _ w _) (Integrate _ _ _ w' _) = compare w w'
    compare (Integrate _ _ _ _ _) _ = GT
    compare (Product _ l h w) (Product _ l' h' w') =
        compare l l' <<>> compare h h' <<>> compare w w'
    compare (Product _ _ _ _) _ = GT

    compare (Sum _ l h w) (Sum _ l' h' w') =
        compare l l' <<>> compare h h' <<>> compare w w'
    compare (Sum _ _ _ _) _ = GT

    compare (App _ _ _) _ = LT

    compare (Block _ _ _) _ = GT
    compare (NumEntity _) _ = LT
    compare f1 f2 = comparing nodeCount f1 f2
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
    getProps OpLazyAttrib = [(AssocSide, OpAssocRight)] 
    getProps OpAttrib = [(AssocSide, OpAssocRight)] 
    getProps OpEq = [(AssocSide, OpAssocRight)] 
    getProps OpCons = [(AssocSide, OpAssocRight)] 
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
emptyProps = map . flip (,)

instance Property BinOperator OpProp BinOperator where
    getProps OpEq  = []
    getProps OpEntail  = []
    getProps OpType  = []

    getProps OpAnd = []
    getProps OpOr = []
    getProps OpNe = []
    getProps OpLe = []
    getProps OpGe = []
    getProps OpLt = []
    getProps OpGt = []

    getProps OpPow = []
    getProps OpAttrib = []
    getProps OpCons = []
    getProps OpLazyAttrib = []

    getProps OpSub = [(InverseOp, OpAdd)]
    getProps OpAdd =
        (InverseOp, OpSub) : emptyProps OpAdd [Associativ, Commutativ]
    getProps OpMul =
        (InverseOp, OpDiv) : emptyProps OpMul [Associativ, Commutativ, Distributiv]
    getProps OpDiv = 
        (InverseOp, OpMul) : emptyProps OpDiv [Distributiv]

canDistributeOver :: BinOperator -> BinOperator -> Bool
canDistributeOver op1 = (`elem` distributeOver op1)

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
    getProps op = [(Priority, first. fromJust $ lookup op binopDefs)]
        where first (f,_,_) = f
    
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
binopDefs :: [(BinOperator, (Int, String, String))]
binopDefs =
    [ (OpAttrib,     (10, ":=", "Attribution operator"))
    , (OpLazyAttrib, (10, ":>", "Lazy attribution operator"))
    , (OpEntail, (9, "|-", "Entailment operator"))
    , (OpType, (8, ":", "Type operator"))
    , (OpCons,(7,  "::", "List appending operator"))
    , (OpAnd, (6,  "&", "Logical and operator"))
    , (OpOr,  (6,  "|", "Logical or operator"))
    , (OpEq,  (5,  "=", "Equality operator"))
    , (OpNe,  (5, "/=", "Different operator"))
    , (OpLt,  (5, "<" , "Lower than operator"))
    , (OpGt,  (5, ">" , "Greater than operator"))
    , (OpGe,  (5, ">=", "Greater or equal operator"))
    , (OpLe,  (5, "<=", "Lower or equal operator"))
    , (OpAdd, (4,  "+", "Addition operator"))
    , (OpSub, (4,  "-", "Substraction operator"))
    , (OpMul, (3,  "*", "Multiplication operator"))
    , (OpDiv, (3,  "/", "Division/fraction operator"))
    , (OpPow, (2,  "^", "Power operator"))
    ]

binopString :: BinOperator -> String
binopString a = second . fromJust $ lookup a binopDefs
    where second (_, s, _) = s

unopString :: UnOperator -> String
unopString a = fromJust $ lookup a unOpNames

realUnopOperators :: [(UnOperator, String, String)]
realUnopOperators = [ (OpNegate, "-", "Negation operator, put it before expression (-x)")
                    , (OpFactorial, "!", "Factorial operator, put it after expression (x!)")
                    ]

-- | Textual representation of "unary" operators
unOpNames :: [(UnOperator, String)]
unOpNames = [ (op, reprez) | (op, reprez,_) <- realUnopOperators] ++
    [ (OpAbs, "abs")
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

    , (OpMatrixWidth, "matrixWidth")
    , (OpMatrixHeight, "matrixHeight")
    ]
 
-------------------------------------------
---- Formula Folding
-------------------------------------------
foldf :: (Monoid b)
      => (FormulaPrim -> b -> b) -> b -> FormulaPrim -> b
foldf f acc m@(Meta _ _ fo) = f m $ foldf f acc fo
foldf f acc fo@(UnOp _ _ sub) = f fo $ foldf f acc sub
foldf f acc fo@(App _ def args) =
    f fo (foldf f listAcc def)
     where listAcc = foldr f acc args

foldf f acc fo@(BinOp _ _ args) =
    f fo $ foldr f acc args

foldf f acc fo@(Sum _ ini end what) = f fo finalAcc
    where whatAcc = foldf f acc what
          iniAcc = foldf f acc ini
          endAcc = foldf f acc end
          finalAcc = whatAcc `mappend` iniAcc `mappend` endAcc

foldf f acc fo@(Product _ ini end what) = f fo finalAcc
        where whatAcc = foldf f acc what
              iniAcc = foldf f acc ini
              endAcc = foldf f acc end
              finalAcc = whatAcc `mappend` iniAcc `mappend` endAcc

foldf f acc fo@(Integrate _ ini end what var) = f fo finalAcc
        where whatAcc = foldf f acc what
              iniAcc = foldf f acc ini
              endAcc = foldf f acc end
              varAcc = foldf f acc var
              finalAcc = whatAcc `mappend` iniAcc 
                                 `mappend` endAcc `mappend` varAcc

foldf f acc fo@(Derivate _ what var) = f fo $ whatAcc `mappend` varAcc
        where whatAcc = foldf f acc what
              varAcc = foldf f acc var

foldf f acc fo@(Matrix _ _ _ cells) = f fo finalAcc
    where lineFolder acc' formu = acc' `mappend` foldf f acc formu
          rowAccs = [ foldl' lineFolder mempty row | row <- cells]
          finalAcc = foldl1' mappend rowAccs

foldf f acc fo = f fo acc

----------------------------------------
----  Strong and valid instances    ----
----------------------------------------
instance Num FormulaPrim where
    (+) = binOpMerger OpAdd
    (-) = binOpMerger OpSub
    (*) = binOpMerger OpMul
    negate = unOp OpNegate
    abs = unOp OpAbs
    signum (CInteger n) = CInteger (signum n)
    signum (CFloat f) = CFloat (signum f)
    signum _ = CInteger 0
    fromInteger = CInteger . fromInteger

instance Fractional FormulaPrim where
    (/) = binOpMerger OpDiv
    recip b = binOp OpDiv [CInteger 1, b]
    fromRational a = binOp OpDiv [ int $ numerator a
                                 , int $ denominator a]
            where int = CInteger . fromInteger
    
instance Floating FormulaPrim where
    pi = CFloat pi 
    exp = unOp OpExp
    sqrt = unOp OpSqrt
    log = unOp OpLn
    (**) = binOpMerger OpPow
    sin = unOp OpSin
    cos = unOp OpCos
    tan = unOp OpTan
    asin = unOp OpASin
    acos = unOp OpACos
    atan = unOp OpATan
    sinh = unOp OpSinh
    cosh = unOp OpCosh
    tanh = unOp OpTanh
    asinh = unOp OpASinh
    acosh = unOp OpACosh
    atanh = unOp OpATanh

