{-# LANGUAGE Rank2Types #-}
module EqManips.Algorithm.Utils ( EmptyMonad( .. )
                                , biAssocM 
                                , biAssoc
                                , asAMonad
                                , fromEmptyMonad 
                                , treeIfyBinOp 
                                , listifyBinOp 
                                , parseFormula
                                ) where
import Text.Parsec.Error( ParseError )
import Text.ParserCombinators.Parsec.Prim( runParser )
import Control.Applicative
import EqManips.Propreties
import EqManips.Types
import EqManips.FormulaIterator
import EqManips.Linker

newtype EmptyMonad a = EmptyMonad a

instance Functor EmptyMonad where
    fmap f (EmptyMonad a) = EmptyMonad $ f a 
    
instance Applicative EmptyMonad where
    pure = EmptyMonad 
    (EmptyMonad f) <*> (EmptyMonad a) = EmptyMonad $ f a

instance Monad EmptyMonad where
    return  = EmptyMonad 
    (EmptyMonad a) >>= b = b a

fromEmptyMonad :: EmptyMonad a -> a
fromEmptyMonad (EmptyMonad a) = a

asAMonad :: (forall m. (Monad m) => (a -> m b) -> a -> m b) -> (a -> b) -> a -> b
asAMonad f a = fromEmptyMonad . f (EmptyMonad . a)


-----------------------------------------------------------
--          Parsing formula
-----------------------------------------------------------
parseFormula :: String -> Either ParseError Formula
parseFormula text = rez
    where parsed = runParser expr () "FromFile" text
          rez = case parsed of
             Left _ -> parsed
             Right f -> Right . listifyFormula $ linkFormula f

listifyFormula :: Formula -> Formula
listifyFormula = depthFirstFormula `asAMonad` listifyBinOp 

listifyBinOp :: Formula -> Formula
listifyBinOp f@(BinOp op _) = BinOp op $ translate f
    where translate = flatten (op `obtainProp` AssocSide)
          flatten OpAssocRight = rightLister
          flatten OpAssocLeft 
                | op `hasProp` Associativ = rightLister . BinOp op . leftLister []
                | otherwise = leftLister []

          leftLister acc (BinOp op' (f1@(BinOp op'' _): f2))
                | op' == op'' = leftLister (f2 ++ acc) f1
          leftLister acc (BinOp op' fl)
                | op' == op = fl ++ acc
          leftLister acc final = final : acc

          rightLister (BinOp op' [f1, f2@(BinOp op'' _)])
                | op' == op'' = f1 : flatten OpAssocRight f2 
          rightLister (BinOp op' fl)
                | op' == op = fl
          rightLister final = [final]

listifyBinOp a = a

treeIfyBinOp :: Formula -> Formula
treeIfyBinOp (BinOp _ []) = error "Impossible to treeIfy"
treeIfyBinOp f@(BinOp _ [_,_]) = f
treeIfyBinOp (BinOp op (f1:f2:fs)) = innerNode $ op `obtainProp` AssocSide
        where innerNode OpAssocLeft = BinOp op $ (BinOp op [f1, f2]) : fs
              innerNode OpAssocRight = BinOp op [f1, BinOp op $ f2 : fs]
treeIfyBinOp _ = error "treeIfy of non binop formula"

biAssoc :: (a -> a -> Either a (a,a)) -> [a] -> [a]
biAssoc _ [] = []
biAssoc _ [x] = [x]
biAssoc f [x,y] = case f x y of
    Left v -> [v]
    Right (v1, v2) -> [v1, v2]
biAssoc f (x:y:xs) = case f x y of
    Left v -> biAssoc f (v:xs)
    Right (v1, v2) -> v1 : biAssoc f (v2:xs)

biAssocM :: (Monad m) => (a -> a -> m (Either a (a,a))) -> [a] -> m [a]
biAssocM f lst = assocInner lst
    where assocInner [] = return []
          assocInner [x] = return [x]
          assocInner [x,y] = f x y >>= \val -> case val of
              Left v -> return [v]
              Right (v1, v2) -> return [v1, v2]
          assocInner (x:y:xs) = f x y >>= \val -> case val of
              Left v -> assocInner (v:xs)
              Right (v1, v2) -> assocInner (v2:xs) >>= return . (v1:) 
