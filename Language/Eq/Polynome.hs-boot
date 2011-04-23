module EqManips.Polynome where

import {-# SOURCE #-} EqManips.Types

convertToPolynome :: Formula ListForm -> Maybe Polynome
convertToFormula :: Polynome -> Formula ListForm
polyMap :: ((PolyCoeff, Polynome) -> (PolyCoeff, Polynome)) -> Polynome -> Polynome

