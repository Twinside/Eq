module EqManips.Algorithm.Expand ( expand ) where

import Data.List( foldl' )
import EqManips.Types
import EqManips.Algorithm.Utils
import EqManips.FormulaIterator
import EqManips.Propreties

expand :: Formula -> Formula
expand f = listifyBinOp $ depthFirstFormula `asAMonad` expander $ f

expander :: Formula -> Formula
expander (BinOp op args)
    | op `hasProp` Distributiv = Variable ""-- foldl' (distribute op) args
expander f = f

distribute :: BinOperator -> Formula -> Formula -> Formula
distribute op formula (BinOp op' nargs) = Variable ""
distribute op formula with = BinOp op [formula, with]

