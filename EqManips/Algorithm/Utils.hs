-- | Utility function/types used in the project.
module EqManips.Algorithm.Utils ( biAssocM, biAssoc
                                , asAMonad
                                , fromEmptyMonad 
                                , treeIfyFormula,  treeIfyBinOp 
                                , listifyFormula, listifyBinOp 
                                , isFormulaConstant, isFormulaConstant' 
                                , isFormulaInteger 
                                , parseFormula
                                , parseProgramm 
                                , sortFormula, invSortFormula 

                                , nodeCount     -- ^ Count nodes in basic formula
                                , nodeCount'    -- ^ Same version with form info.
                                , needParenthesis 
                                , needParenthesisPrio 
                                , interspereseS 
                                , concatS 
                                , concatMapS 
                                , collectSymbols, collectSymbols'
                                ) where

import qualified Data.Monoid as Monoid
import Data.Monoid( All( .. ), mempty )
import Text.Parsec.Error( ParseError )
import Text.ParserCombinators.Parsec.Prim( runParser )
import EqManips.Algorithm.EmptyMonad
import EqManips.Propreties
import EqManips.Types
import EqManips.FormulaIterator
import EqManips.Linker
import Data.List( foldl', sort, sortBy )

-----------------------------------------------------------
--          Parsing formula
-----------------------------------------------------------
-- | Helper function to parse a formula and apply all
-- needed algorithm to be able to apply them
parseFormula :: String -> Either ParseError (Formula ListForm)
parseFormula text = case runParser expr () "FromFile" text of
             Left e -> Left e
             Right f -> Right . listifyFormula
                              . linkFormula
                              $ Formula f

-- | Count the number of nodes in a formula.
nodeCount :: FormulaPrim -> Int
nodeCount f = Monoid.getSum $ foldf 
   (\_ a -> Monoid.Sum $ Monoid.getSum a + 1)
   (Monoid.Sum 0) f

nodeCount' :: Formula anyForm -> Int
nodeCount' (Formula a) = nodeCount a

-- | Perform a semantic sorting on formula, trying to put numbers
-- front and rassembling terms
sortFormula :: Formula ListForm -> Formula ListForm
sortFormula (Formula a) = Formula $ (depthFormulaPrimTraversal `asAMonad` sortBinOp) a

-- | Sort a binary operator, used by sortFormula to sort globally
-- a formula
sortBinOp :: FormulaPrim -> FormulaPrim
sortBinOp (BinOp op lst)
    | op `hasProp` Associativ && op `hasProp` Commutativ = BinOp op $ sort lst
sortBinOp a = a

invSortFormula :: Formula ListForm -> Formula ListForm
invSortFormula (Formula a) =
    Formula $ (depthFormulaPrimTraversal `asAMonad` invSortBinOp) a

invSortBinOp :: FormulaPrim -> FormulaPrim
invSortBinOp (BinOp op lst)
    | op `hasProp` Associativ && op `hasProp` Commutativ = BinOp op $ sortBy cmp lst
        where cmp a b = invOrd $ compare a b
              invOrd GT = LT
              invOrd LT = GT
              invOrd EQ = EQ
invSortBinOp a = a

-- | Helper function to use to parse a programm.
-- Perform some transformations to get a usable
-- formula.
parseProgramm :: String -> Either ParseError [Formula ListForm]
parseProgramm text = rez
    where parsed = runParser program () "FromFile" text
          rez = case parsed of
                 Left a -> Left a
                 Right f -> Right $ map (listifyFormula . linkFormula . Formula) f

-- | listify a whole formula
listifyFormula :: Formula TreeForm -> Formula ListForm
listifyFormula (Formula a) = Formula $
    (depthFormulaPrimTraversal `asAMonad` listifyBinOp) a


-- | Given a binary operator in binary tree form,
-- transform it in list form.
listifyBinOp :: FormulaPrim -> FormulaPrim
listifyBinOp (BinOp op lst) = BinOp op $ translate lst
    where translate = flatten (op `obtainProp` AssocSide)
          flatten OpAssocRight = rightLister
          flatten OpAssocLeft 
                | op `hasProp` Associativ = rightLister . leftLister
                | otherwise = leftLister

          leftLister = foldr lefter []

          -- left associative operator packing.
          lefter (BinOp op' fl) acc
                | op == op' = foldr lefter acc fl
          lefter final acc = final : acc

          rightLister = foldl' righter []
          -- right associative operator packing.
          righter acc (BinOp op' fl)
                | op' == op = foldl' righter acc fl
          righter acc e = acc ++ [e]

listifyBinOp a = a

-- | treeify a whole formula
treeIfyFormula :: Formula ListForm -> Formula TreeForm
treeIfyFormula (Formula a) = Formula f
    where f :: FormulaPrim
          f = depthFormulaPrimTraversal `asAMonad` treeIfyBinOp $ a

-- | Given a formula where all binops are in list
-- forms, transform it back to binary tree.
treeIfyBinOp :: FormulaPrim -> FormulaPrim
treeIfyBinOp (BinOp _ []) = error "treeIfyBinOp - empty binop"
treeIfyBinOp (BinOp _ [_]) = error "treeIfyBinOp - Singleton binop"
treeIfyBinOp f@(BinOp _ [_,_]) = f
treeIfyBinOp (BinOp op lst) = innerNode (op `obtainProp` AssocSide) lst
        where innerNode OpAssocLeft (fx:fy:fs) = 
                foldl' innerLeft (BinOp op [fx, fy]) fs
              innerNode OpAssocRight lst' = innerRight lst'
              innerNode _ _ = error "treeIfyBinOp - weird unhandled case"

              innerRight [a,b] = BinOp op [a,b]
              innerRight (fx:fs) = BinOp op [fx, innerRight fs]
              innerRight _ = error "treeIfyBinOp - bleh right"

              innerLeft acc fx = BinOp op [acc, fx]
treeIfyBinOp f = f

-- | Little helper to help to know if a formula renderer
-- need to put parenthesis around the current node regarding
-- his parent node.
needParenthesis :: Bool         -- ^ if the node is on the right side of parent operator
                -> BinOperator  -- ^ Parent operator
                -> BinOperator  -- ^ This node operator
                -> Bool
needParenthesis v parentOp op =
    needParenthesisPrio v (parentOp `obtainProp` Priority) op

-- | Little helper to know if a renderer need to put parenthesis
-- given his parent's priority
needParenthesisPrio :: Bool        -- ^ If the node is on the right side of parent operator
                    -> Int         -- ^ Parent operator priority
                    -> BinOperator -- ^ This node operator
                    -> Bool
needParenthesisPrio True parentPrio op =
    (op `obtainProp` Priority) >= parentPrio
needParenthesisPrio False parentPrio op =
    (op `obtainProp` Priority) > parentPrio

-- | Bi associate operation on a list of elements.
-- Can be used for reduction of formula.
biAssoc :: (a -> a -> Either a (a,a)) 
        -> (a -> a -> Either a (a,a)) 
        -> [a] -> [a]
biAssoc f finv lst = fromEmptyMonad 
                   $ biAssocM (\a -> return . f a) 
                              (\a -> return . finv a)
                              lst

-- | same as biAssoc, but use monads.
biAssocM :: (Monad m) 
         => (a -> a -> m (Either a (a,a))) 
         -> (a -> a -> m (Either a (a,a))) 
         -> [a] -> m [a]
biAssocM f finv lst = assocInner f lst
    where assocInner _ [] = return []
          assocInner _ [x] = return [x]
          assocInner f' [x,y] = f' x y >>= \val -> case val of
              Left v -> return [v]
              Right (v1, v2) -> return [v1, v2]
          assocInner f' (x:y:xs) = f' x y >>= \val -> case val of
              Left v -> assocInner f' (v:xs)
              Right (v1, v2) -> assocInner finv (v2:xs) >>= return . (v1:) 

-- | Work like concat on list, but instead
-- just combine functions of kind of ShowS.
-- The function is generalized
concatS :: [a -> a] -> (a -> a)
concatS = foldr1 (.)

-- | Work like concatMap, but instead use 
-- function combination.
concatMapS :: (a -> b -> b) -> [a] -> (b -> b)
concatMapS f = concatS . map f

-- | Same functionality as intersperse but combine function
-- instead of concatenation
interspereseS :: (a -> a) -> [a -> a] -> a -> a
interspereseS what within =
   foldl' (\acc e -> e . what . acc) lastOne reversed
    where (lastOne : reversed) = reverse within

-- | Collect all the symbols present in the formula.
-- Symbols can be present multiple times
collectSymbols :: FormulaPrim -> [String]
collectSymbols = foldf symbolCollector []
    where symbolCollector (Variable v) acc = v:acc
          symbolCollector _ acc = acc

collectSymbols' :: Formula anyKind -> [String]
collectSymbols' (Formula a) = collectSymbols a

isFormulaInteger :: FormulaPrim -> Bool
isFormulaInteger = getAll . foldf isConstant mempty
    where isConstant (Variable _) _ = All False
          isConstant (Sum _ _ _) _ = All False
          isConstant (Product _ _ _) _ = All False
          isConstant (Derivate _ _) _ = All False
          isConstant (Integrate _ _ _ _) _ = All False
          isConstant (Lambda _) _ = All False
          isConstant (App _ _) _ = All False
          isConstant (Block _ _ _) _ = All False
          --
          isConstant (CFloat _) _ = All False
          isConstant (CInteger _) _ = All True
          isConstant (Truth _) _ = All False
          isConstant (NumEntity _) _ = All False
          --
          isConstant (UnOp op _) a = isValidUnop op a
          isConstant (BinOp _ _) a = a
          isConstant (Meta _ _) a = a
          isConstant (Matrix 1 1 _) a = a
          isConstant (Matrix _ _ _) _ = All False

          isValidUnop OpNegate a = a
          isValidUnop OpAbs a = a
          isValidUnop OpFactorial _ = All True
          isValidUnop OpCeil _ = All True
          isValidUnop OpFloor _ = All True
          isValidUnop _ _ = All False


-- | Tell if a formula can be reduced to a scalar somehow
isFormulaConstant :: FormulaPrim -> Bool
isFormulaConstant = getAll . foldf isConstant mempty
    where isConstant (Variable _) _ = All False
          isConstant (Sum _ _ _) _ = All False
          isConstant (Product _ _ _) _ = All False
          isConstant (Derivate _ _) _ = All False
          isConstant (Integrate _ _ _ _) _ = All False
          isConstant (Lambda _) _ = All False
          isConstant (App _ _) _ = All False
          isConstant (Block _ _ _) _ = All False
          --
          isConstant (CFloat _) _ = All True
          isConstant (CInteger _) _ = All True
          isConstant (Truth _) _ = All True
          isConstant (NumEntity _) _ = All True
          --
          isConstant (UnOp _ _) a = a
          isConstant (BinOp _ _) a = a
          isConstant (Meta _ _) a = a
          isConstant (Matrix 1 1 _) a = a
          isConstant (Matrix _ _ _) _ = All False

-- | Tell if a formula in any form can be reduced
-- to a scalar somehow
isFormulaConstant' :: Formula anyKind -> Bool
isFormulaConstant' (Formula a) = isFormulaConstant a

