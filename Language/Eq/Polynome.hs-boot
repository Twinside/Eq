module Language.Eq.Polynome where

import {-# SOURCE #-} Language.Eq.Types

convertToPolynome :: Formula ListForm -> Maybe Polynome
convertToFormula :: Polynome -> Formula ListForm
polyMap :: ((PolyCoeff, Polynome) -> (PolyCoeff, Polynome)) -> Polynome -> Polynome

