module EqManips.Algorithm.EvalTypes( EvalOp
                                   , EvalFun
                                   , FormulOperator
                                   , EvalPredicate
                                   , FormulaEvaluator
                                   , taggedEvaluator, deTagEvaluator 
                                   ) where

import EqManips.Types
import EqManips.EvaluationContext

type EvalOp = FormulaPrim
            -> FormulaPrim
            -> EqContext (Either FormulaPrim (FormulaPrim,FormulaPrim))

-- | Type for formula evaluating functions
type EvalFun = FormulaPrim -> EqContext FormulaPrim

-- | Same as EvalFun, but is lingua franca for tagged formula.
type FormulaEvaluator = Formula ListForm -> EqContext (Formula ListForm)

-- | A low-level predicate
type EvalPredicate = FormulaPrim -> FormulaPrim -> Maybe Bool

-- | A binary operator for formula
type FormulOperator = FormulaPrim -> FormulaPrim -> FormulaPrim


-- | Transform an EvalFun to it's tagged counterpart. Just
-- to please the type system.
taggedEvaluator :: EvalFun -> FormulaEvaluator
taggedEvaluator evaluator (Formula a)= do 
    evaluated <- evaluator a
    return $ Formula evaluated

deTagEvaluator :: FormulaEvaluator -> EvalFun
deTagEvaluator eval f = do
    evaluated <- eval $ Formula f
    return $ unTagFormula evaluated


