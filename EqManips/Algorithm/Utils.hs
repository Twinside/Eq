-- | Utility function/types used in the project.
module EqManips.Algorithm.Utils ( biAssocM, biAssoc
                                , asAMonad
                                , fromEmptyMonad 
                                , treeIfyFormula,  treeIfyBinOp 
                                , listifyFormula, listifyBinOp 
                                , isFormulaConstant, isFormulaConstant' 
                                , isFormulaInteger, isFormulaScalar 
                                , sortFormula, invSortFormula, sortBinOp  
                                
                                -- | Count nodes in basic formula
                                , nodeCount     
                                -- | Same version with form info.
                                , nodeCount'    
                                , needParenthesis 
                                , needParenthesisPrio 
                                , interspereseS 
                                , concatS 
                                , concatMapS 
                                , collectSymbols, collectSymbols'

                                -- | Translate complex into "simpler" format,
                                -- intended for display use only!
                                , complexTranslate 
                                ) where

import Control.Applicative
import qualified Data.Monoid as Monoid

import Data.Monoid( All( .. ), mempty )
import EqManips.Algorithm.EmptyMonad
import EqManips.Propreties
import EqManips.Types
import {-# SOURCE #-} EqManips.FormulaIterator
import Data.List( foldl', sortBy )

-----------------------------------------------------------
--          Parsing formula
-----------------------------------------------------------
-- | Count the number of nodes in a formula.
nodeCount :: FormulaPrim -> Int
nodeCount = Monoid.getSum . foldf 
   (\_ a -> Monoid.Sum $ Monoid.getSum a + 1)
   (Monoid.Sum 0)

nodeCount' :: Formula anyForm -> Int
nodeCount' (Formula a) = nodeCount a

-- | Perform a semantic sorting on formula, trying to put numbers
-- front and rassembling terms
sortFormula :: Formula ListForm -> Formula ListForm
sortFormula (Formula a) = Formula 
                        $ (depthFormulaPrimTraversal `asAMonad` sortBinOp compare) a

-- | Sort a binary operator, used by sortFormula to sort globally
-- a formula
sortBinOp :: (FormulaPrim -> FormulaPrim -> Ordering) -> FormulaPrim -> FormulaPrim
sortBinOp f (BinOp _ op lst)
    | op `hasProp` Associativ && op `hasProp` Commutativ = binOp op $ sortBy f lst
sortBinOp _f a = a

invSortFormula :: Formula ListForm -> Formula ListForm
invSortFormula (Formula f) =
    Formula $ (depthFormulaPrimTraversal `asAMonad` sortBinOp cmp) f
        where cmp a = invOrd . compare a
              invOrd GT = LT
              invOrd LT = GT
              invOrd EQ = EQ

-- | listify a whole formula
listifyFormula :: Formula TreeForm -> Formula ListForm
listifyFormula (Formula a) = Formula $
    (depthFormulaPrimTraversal `asAMonad` listifyBinOp) a


-- | Given a binary operator in binary tree form,
-- transform it in list form.
listifyBinOp :: FormulaPrim -> FormulaPrim
listifyBinOp (BinOp _ op lst) = binOp op $ translate lst
    where translate = flatten (op `obtainProp` AssocSide)
          flatten OpAssocRight = rightLister
          flatten OpAssocLeft 
                | op `hasProp` Associativ = rightLister . leftLister
                | otherwise = leftLister

          leftLister = foldr lefter []

          -- left associative operator packing.
          lefter (BinOp _ op' fl) acc
                | op == op' = foldr lefter acc fl
          lefter final acc = final : acc

          rightLister = foldl' righter []
          -- right associative operator packing.
          righter acc (BinOp _ op' fl)
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
treeIfyBinOp (BinOp _ _ []) = error "treeIfyBinOp - empty binop"
treeIfyBinOp f@(BinOp _ _ [_]) = error ("treeIfyBinOp - Singleton binop " ++ show f)
treeIfyBinOp f@(BinOp _ _ [_,_]) = f
treeIfyBinOp (BinOp _ op lst) = innerNode (op `obtainProp` AssocSide) lst
        where innerNode OpAssocLeft (fx:fy:fs) = 
                foldl' innerLeft (binOp op [fx, fy]) fs
              innerNode OpAssocRight lst' = innerRight lst'
              innerNode _ _ = error "treeIfyBinOp - weird unhandled case"

              innerRight [a,b] = binOp op [a,b]
              innerRight (fx:fs) = binOp op [fx, innerRight fs]
              innerRight _ = error "treeIfyBinOp - bleh right"

              innerLeft acc fx = binOp op [acc, fx]
treeIfyBinOp f = f

-- | Little helper to help to know if a formula renderer
-- need to put parenthesis around the current node regarding
-- his parent node.
needParenthesis :: Bool         -- ^ if the node is on the right side of parent operator
                -> BinOperator  -- ^ Parent operator
                -> BinOperator  -- ^ This node operator
                -> Bool
needParenthesis v =
    needParenthesisPrio v . (`obtainProp` Priority)

-- | Little helper to know if a renderer need to put parenthesis
-- given his parent's priority
needParenthesisPrio :: Bool        -- ^ If the node is on the right side of parent operator
                    -> Int         -- ^ Parent operator priority
                    -> BinOperator -- ^ This node operator
                    -> Bool
-- for right associative operators, it's reversed.
needParenthesisPrio True parentPrio op
    | op `obtainProp` AssocSide == OpAssocRight =
        (op `obtainProp` Priority) > parentPrio
    | otherwise =
        (op `obtainProp` Priority) >= parentPrio

needParenthesisPrio False parentPrio op
    | op `obtainProp` AssocSide == OpAssocRight =
        (op `obtainProp` Priority) >= parentPrio
    | otherwise =
        (op `obtainProp` Priority) > parentPrio

-- | Bi associate operation on a list of elements.
-- Can be used for reduction of formula.
biAssoc :: (a -> a -> Either a (a,a)) 
        -> (a -> a -> Either a (a,a)) 
        -> [a] -> [a]
biAssoc f finv = fromEmptyMonad 
               . biAssocM (\a -> return . f a) 
                          (\a -> return . finv a)

-- | same as biAssoc, but use monads.
{-
{-# SPECIALIZE biAssocM :: (FormulaPrim -> FormulaPrim -> EqContext (Either FormulaPrim (FormulaPrim,FormulaPrim))) 
                        -> (FormulaPrim -> FormulaPrim -> EqContext (Either FormulaPrim (FormulaPrim,FormulaPrim)))
                        -> [FormulaPrim] -> EqContext [FormulaPrim] #-}
                        -}
biAssocM :: (Monad m, Functor m)
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
              Right (v1, v2) -> (v1:) <$> assocInner finv (v2:xs)

-- | Work like concat on list, but instead
-- just combine functions of kind of ShowS.
-- The function is generalized
concatS :: [a -> a] -> (a -> a)
concatS []  = id
concatS lst = foldr1 (.) lst

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
          isConstant (Sum _ _ _ _) _ = All False
          isConstant (Poly _ _) _ = All False
          isConstant (Product _ _ _ _) _ = All False
          isConstant (Derivate _ _ _) _ = All False
          isConstant (Integrate _ _ _ _ _) _ = All False
          isConstant (Lambda _ _) _ = All False
          isConstant (App _ _ _) _ = All False
          isConstant (Block _ _ _) _ = All False
          --
          isConstant (CFloat _) _ = All False
          isConstant (CInteger _) _ = All True
          isConstant (Complex _ _) _ = All False
          isConstant (Fraction _) _ = All True
          isConstant (Truth _) _ = All False
          isConstant (NumEntity _) _ = All False
          --
          isConstant (UnOp _ op _) a = isValidUnop op a
          isConstant (BinOp _ _ _) a = a
          isConstant (Meta _ _ _) a = a
          isConstant (Matrix _ 1 1 _) a = a
          isConstant (Matrix _ _ _ _) _ = All False
          isConstant (Indexes _ _ _) _ = All False
          isConstant (List _ _) _ = All False

          isValidUnop OpNegate a = a
          isValidUnop OpAbs a = a
          isValidUnop OpFactorial _ = All True
          isValidUnop OpCeil _ = All True
          isValidUnop OpFloor _ = All True
          isValidUnop _ _ = All False

isFormulaScalar :: FormulaPrim -> Bool
isFormulaScalar (CFloat _) = True
isFormulaScalar (CInteger _) = True
isFormulaScalar (Fraction _) = True
isFormulaScalar (Complex _ (a,b)) = isFormulaScalar a && isFormulaScalar b
isFormulaScalar (UnOp _ OpNegate f) = isFormulaScalar f
isFormulaScalar _ = False

-- | Translate a complex to a simpler formula using '+' and '*'
-- Perform mandatory simplification
complexTranslate :: (FormulaPrim, FormulaPrim) -> FormulaPrim
complexTranslate (a,b)
    | b == CInteger 0 || b == CFloat 0.0 = a
    | a == CInteger 0 || a == CFloat 0.0 = Variable "i" * b
    | otherwise = a + Variable "i" * b

-- | Tell if a formula can be reduced to a scalar somehow
isFormulaConstant :: FormulaPrim -> Bool
isFormulaConstant = getAll . foldf isConstant mempty
    where isConstant (Variable _) _ = All False
          isConstant (Poly _ _) _ = All False
          isConstant (Sum _ _ _ _) _ = All False
          isConstant (Product _ _ _ _) _ = All False
          isConstant (Derivate _ _ _) _ = All False
          isConstant (Integrate _ _ _ _ _) _ = All False
          isConstant (Lambda _ _) _ = All False
          isConstant (App _ _ _) _ = All False
          isConstant (Block _ _ _) _ = All False
          --
          isConstant (CFloat _) _ = All True
          isConstant (CInteger _) _ = All True
          isConstant (Truth _) _ = All True
          isConstant (NumEntity _) _ = All True
          isConstant (Fraction _) _ = All True
          isConstant (List _ _) _ = All False
          isConstant (Indexes _ _ _) _ = All False

          --
          isConstant (Complex _ _) a = a
          isConstant (UnOp _ _ _) a = a
          isConstant (BinOp _ _ _) a = a
          isConstant (Meta _ _ _) a = a
          isConstant (Matrix _ 1 1 _) a = a
          isConstant (Matrix _ _ _ _) _ = All False

-- | Tell if a formula in any form can be reduced
-- to a scalar somehow
isFormulaConstant' :: Formula anyKind -> Bool
isFormulaConstant' (Formula a) = isFormulaConstant a

