{-# LANGUAGE RoleAnnotations #-}
module Language.Eq.Types where

data FormulaPrim

type role Formula phantom
newtype Formula formulaForm = Formula { unTagFormula :: FormulaPrim }

data ListForm
data PolyCoeff
data Polynome

