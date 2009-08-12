module EqManips.Algorithm.Eval( reduce
                              , runProgramm
                              , evalGlobalStatement 
                              ) where

import EqManips.Types
import EqManips.EvaluationContext

runProgramm :: [Formula] -> EqContext [Formula]

reduce :: Formula -> EqContext Formula

evalGlobalStatement :: Formula -> EqContext Formula

