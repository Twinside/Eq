module EqManips.Algorithm.Derivative( derivate
                                    , Var ) where

import EqManips.Types
import EqManips.EvaluationContext

type Var = String

derivate :: Formula -> Var -> EqContext Formula

