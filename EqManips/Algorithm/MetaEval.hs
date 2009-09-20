module EqManips.Algorithm.MetaEval ( metaEval ) where

import EqManips.Algorithm.Utils
import EqManips.Algorithm.Expand
import EqManips.Types
import EqManips.EvaluationContext
import {-# SOURCE #-} EqManips.Algorithm.Eval

metaEval :: MetaOperation -> Formula -> EqContext Formula
metaEval Force f = reduce f
metaEval Hold f = return f
metaEval Listify f = return . listifyBinOp =<< reduce f
metaEval Treefy f = return . treeIfyBinOp =<< reduce f
metaEval Expand f = return $ expand f

