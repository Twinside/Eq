{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Eq.Quasiquote( eqDefs ) where

import Control.Applicative

import Data.List( foldl' )
import Language.Eq.Algorithm.Eval
import Language.Eq.Types
import Language.Eq.EvaluationContext
import Language.Eq.InputParser.EqCode

import qualified Data.Map as M

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

instance Applicative Q where
    pure = return
    a <*> b = do
        a' <- a
        b' <- b
        return $ a' b'

-- | Quasi quote transforming Eq code into a symbol list
-- of type :: (String, Formula ListForm)
-- Usefull to prepare a pre-feed symbol table.
eqDefs :: QuasiQuoter
eqDefs = QuasiQuoter { quoteExp = symbolTableExtractor
                     , quotePat = undefined
                     , quoteType = undefined
                     , quoteDec = undefined
                     }

symbolTableExtractor :: String -> Q Exp
symbolTableExtractor str = case parseProgramm str of
    Left _   -> error "Cannot parse the quasi quoted 'Eq' expression"
    Right flist -> [e| elemList |]
        where elemList = M.assocs $ context info
              info = performLastTransformationWithContext M.empty 
                   $ mapM evalGlobalLosslessStatement flist 


constr :: String -> Exp -> Exp
constr str = AppE (ConE $ mkName str)

recConstr :: String -> Int -> Exp
recConstr str i = AppE (ConE $ mkName str) 
                       (LitE . IntegerL $ toInteger i)

appList :: Exp -> [Exp] -> Exp
appList = foldl' AppE

instance Lift Double where
    lift d = return . LitE . DoublePrimL $ toRational d

instance Lift BinOperator where
    lift = return . ConE . mkName . show

instance Lift UnOperator where
    lift = return . ConE . mkName . show

instance Lift MetaOperation where
    lift = return . ConE . mkName . show

instance Lift Entity where
    lift = return . ConE . mkName . show

instance Lift (Formula ListForm) where
    lift (Formula f) = constr "Formula" <$> lift f

instance Lift (Formula TreeForm) where
    lift (Formula f) = constr "Formula" <$> lift f

instance Lift PolyCoeff where
    lift (CoeffFloat f) =
        AppE (ConE $ mkName "CoeffFloat") <$> lift f
    lift (CoeffInt i) =
        AppE (ConE $ mkName "CoeffInt") <$> lift i
    lift (CoeffRatio r) =
        return . AppE (ConE $ mkName "CoeffRatio") . LitE $ RationalL r

instance Lift Polynome where
    lift (Polynome s lst) =
        AppE . (constr "Polynome") <$> lift s
                                   <*> lift lst
    lift (PolyRest c) =
        constr "PolyRest" <$> lift c

instance Lift FormulaPrim where
    lift (Variable str) =
        constr "Variable" <$> lift str
    lift (NumEntity entity) =
        constr "NumEntity" <$> lift entity
    lift (Truth b) =
        constr "Truth" <$> lift b
    lift (CInteger i) =
        constr "CInteger" <$> lift i
    lift (CFloat f) =
        constr "CFloat" <$> lift f

    lift (Fraction r) =
        return . constr "Fraction" . LitE $ RationalL r
    
    lift (Complex i (e1, e2)) =
        AppE <$> (AppE (recConstr "Complex" i) <$> lift e1)
             <*> (lift e2)
    
    lift (Indexes i e el) =
        AppE <$> (AppE (recConstr "Indexes" i) <$> lift e)
             <*> (lift el)
    
    lift (List i el) =
        AppE (recConstr "List" i) <$> (lift el)
    lift (App i e el) =
        AppE <$> (AppE (recConstr "App" i) <$> lift e)
             <*> (lift el)
    
    lift (Sum i e1 e2 e3) =
        appList (recConstr "Sum" i) <$> mapM lift [e1, e2, e3]
    lift (Product i e1 e2 e3) =
        appList (recConstr "Product" i) <$> mapM lift [e1, e2, e3]
    lift (Derivate i e1 e2) =
        appList (recConstr "Derivate" i) <$> mapM lift [e1, e2]
    lift (Integrate i e1 e2 e3 e4) =
        appList (recConstr "Integrate" i) <$> mapM lift [e1, e2, e3, e4]
    lift (UnOp i op e) =
        appList (recConstr "UnOp" i) <$> sequence [lift op, lift e]
    lift (Lambda i lst) =
        AppE (recConstr "Lambda" i) <$> lift lst

    lift (BinOp i op el) =
        AppE <$> (AppE (recConstr "BinOp" i) <$> lift op)
             <*> lift el
    lift (Matrix i n m el) =
        appList (recConstr "Matrix" i) <$> sequence [lift n, lift m, lift el]
    lift (Poly i p) =
        AppE (recConstr "Poly" i) <$> lift p

    lift (Block i1 i2 i3) =
        appList (ConE $ mkName "Block") <$> mapM lift [i1, i2, i3]

    lift (Meta i op sub) =
        appList (recConstr "Meta" i) <$> sequence [lift op, lift sub]

