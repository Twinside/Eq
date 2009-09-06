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
import Data.List( sort )

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
listifyBinOp f@(BinOp op _) = BinOp op $ translate f
    where translate = flatten (op `obtainProp` AssocSide)
          flatten OpAssocRight = rightLister
          flatten OpAssocLeft 
                | op `hasProp` Associativ = rightLister . BinOp op . leftLister []
                | otherwise = leftLister []

          -- left associative operator packing.
          leftLister acc (BinOp op' (f1@(BinOp op'' _): f2))
                | op' == op'' = leftLister (f2 ++ acc) f1
          leftLister acc (BinOp op' fl)
                | op' == op = fl ++ acc
          leftLister acc final = final : acc

          -- right associative operator packing.
          rightLister (BinOp op' [f1, f2@(BinOp op'' _)])
                | op' == op'' = f1 : flatten OpAssocRight f2 
          rightLister (BinOp op' fl)
                | op' == op = fl
          rightLister final = [final]

listifyBinOp a = a

-- | treeify a whole formula
treeIfyFormula :: Formula -> Formula
treeIfyFormula = depthFirstFormula `asAMonad` treeIfyBinOp

-- | Given a formula where all binops are in list
-- forms, transform it back to binary tree.
treeIfyBinOp :: Formula -> Formula
treeIfyBinOp (BinOp _ []) = error "Impossible to treeIfy"
treeIfyBinOp f@(BinOp _ [_,_]) = f
treeIfyBinOp (BinOp op (f1:f2:fs)) = innerNode $ op `obtainProp` AssocSide
        where innerNode OpAssocLeft = BinOp op $ (BinOp op [f1, f2]) : fs
              innerNode OpAssocRight = BinOp op [f1, BinOp op $ f2 : fs]
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

