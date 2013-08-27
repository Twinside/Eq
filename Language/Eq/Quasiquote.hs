{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Eq.Quasiquote( eqDefs ) where

import Language.Eq.Algorithm.Eval
import Language.Eq.Types
import Language.Eq.EvaluationContext
import Language.Eq.InputParser.EqCode

import qualified Data.Map as M

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

-- | Quasi quote transforming Eq code into a symbol list
-- of type :: (String, Formula ListForm)
-- Usefull to prepare a pre-feed symbol table.
-- To use it, yout must use the following :
--
-- @
-- -- at the top of the file.
-- {-# LANGUAGE QuasiQuotes #-}
-- ...
-- -- in any expression
-- [eqDefs| myFunc(a,b) :> a ^ 2 + if(b < 0, 2, 3) |]
--
-- -- you can put several definitions
-- [eqDefs| myFunc(a,b) :> a ^ 2 + if(b < 0, 2, 3);
--          myOtherFunc(a) :> listFromTo(0, a) |]
-- @
-- 
-- Compilation will fail if an error is found in the eq
-- syntax, giving you a (rather succint) error message
-- with some position information in the quotation.
eqDefs :: QuasiQuoter
eqDefs = QuasiQuoter { quoteExp = symbolTableExtractor
                     , quotePat = undefined
                     , quoteType = undefined
                     , quoteDec = undefined
                     }

symbolTableExtractor :: String -> Q Exp
symbolTableExtractor str = case parseProgramm str of
    Left err -> fail $ "Cannot parse the quasi quoted 'Eq' expression"
             ++ show err
    Right flist -> [e| elemList |]
        where elemList = M.assocs $ context info
              info = performLastTransformationWithContext M.empty 
                   $ mapM evalGlobalLosslessStatement flist 


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
    lift (Formula f) = [| Formula f |] 

instance Lift (Formula TreeForm) where
    lift (Formula f) = [| Formula f |]

instance Lift Rational where
    lift = return . LitE . RationalL

instance Lift PolyCoeff where
    lift (CoeffFloat f) = [| CoeffFloat f |]
    lift (CoeffInt i) = [| CoeffInt i |]
    lift (CoeffRatio r) = [| CoeffRatio r |]

instance Lift Polynome where
    lift (Polynome s lst) = [| Polynome s lst |]
    lift (PolyRest c) = [| PolyRest c |]

instance Lift FormulaPrim where
    lift (Variable str) = [| Variable str |]
    lift (NumEntity entity) = [| NumEntity entity |]
    lift (Truth b) = [| Truth b |]
    lift (CInteger i) = [| CInteger i |]
    lift (CFloat f) = [| CFloat f |]
    lift (Fraction r) = [| Fraction r |]
    lift (Complex i (e1, e2)) = [| Complex i (e1, e2) |]
    lift (Indexes i e el) = [| Indexes i e el |]
    lift (List i el) = [| List i el |]
    lift (App i e el) = [| App i e el |]
    lift (Sum i e1 e2 e3) = [| Sum i e1 e2 e3 |]
    lift (Product i e1 e2 e3) = [| Product i e1 e2 e3 |]
    lift (Derivate i e1 e2) = [| Derivate i e1 e2 |]
    lift (Integrate i e1 e2 e3 e4) = [| Integrate i e1 e2 e3 e4 |]
    lift (UnOp i op e) = [| UnOp i op e |]
    lift (Lambda i lst) = [| Lambda i lst |]
    lift (BinOp i op el) = [| BinOp i op el |]
    lift (Matrix i n m el) = [| Matrix i n m el |]
    lift (Poly i p) = [| Poly i p |]
    lift (Block i1 i2 i3) = [| Block i1 i2 i3 |]
    lift (Meta i op sub) = [| Meta i op sub |]
    lift (Infer i h d) = [| Infer i h d |]

