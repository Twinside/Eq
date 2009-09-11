-- | Utility function/types used in the project.
module EqManips.Algorithm.Utils ( biAssocM, biAssoc
                                , asAMonad
                                , fromEmptyMonad 
                                , treeIfyFormula,  treeIfyBinOp 
                                , listifyFormula, listifyBinOp 
                                , parseFormula
                                , parseProgramm 
                                , sortFormula 
                                , nodeCount
                                , needParenthesis 
                                , needParenthesisPrio 
                                ) where

import qualified Data.Monoid as Monoid
import Text.Parsec.Error( ParseError )
import Text.ParserCombinators.Parsec.Prim( runParser )
import EqManips.Algorithm.EmptyMonad
import EqManips.Propreties
import EqManips.Types
import EqManips.FormulaIterator
import EqManips.Linker
import Data.List( foldl', sort )

-----------------------------------------------------------
--          Parsing formula
-----------------------------------------------------------
-- | Helper function to parse a formula and apply all
-- needed algorithm to be able to apply them
parseFormula :: String -> Either ParseError Formula
parseFormula text = rez
    where parsed = runParser expr () "FromFile" text
          rez = case parsed of
             Left _ -> parsed
             Right f -> Right . listifyFormula $ linkFormula f

-- | Count the number of nodes in a formula.
nodeCount :: Formula -> Int
nodeCount f = Monoid.getSum $ foldf 
   (\_ a -> Monoid.Sum $ Monoid.getSum a + 1)
   (Monoid.Sum 0) f

-- | Perform a semantic sorting on formula, trying to put numbers
-- front and rassembling terms
sortFormula :: Formula -> Formula
sortFormula = depthFirstFormula `asAMonad` sortBinOp

-- | Sort a binary operator, used by sortFormula to sort globally
-- a formula
sortBinOp :: Formula -> Formula
sortBinOp (BinOp op lst)
    | op `hasProp` Associativ && op `hasProp` Commutativ = BinOp op $ sort lst
sortBinOp a = a

-- | Helper function to use to parse a programm.
-- Perform some transformations to get a usable
-- formula.
parseProgramm :: String -> Either ParseError [Formula]
parseProgramm text = rez
    where parsed = runParser program () "FromFile" text
          rez = case parsed of
                 Left _ -> parsed
                 Right f -> Right $ map (listifyFormula . linkFormula) f

-- | listify a whole formula
listifyFormula :: Formula -> Formula
listifyFormula = depthFirstFormula `asAMonad` listifyBinOp 


-- | Given a binary operator in binary tree form,
-- transform it in list form.
listifyBinOp :: Formula -> Formula
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
treeIfyFormula :: Formula -> Formula
treeIfyFormula = depthFirstFormula `asAMonad` treeIfyBinOp

-- | Given a formula where all binops are in list
-- forms, transform it back to binary tree.
treeIfyBinOp :: Formula -> Formula
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

