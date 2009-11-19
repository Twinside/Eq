module EqManips.Algorithm.Eval.GlobalStatement( evalGlobalStatement ) where

import qualified EqManips.ErrorMessages as Err
import EqManips.Types
import EqManips.EvaluationContext

import EqManips.Algorithm.Eval.Types


-- | Add a function into the symbol table.
addLambda :: String -> [Formula ListForm] -> Formula ListForm -> EqContext ()
addLambda varName args body = do
    symb <- symbolLookup varName
    case symb of
      Nothing -> addSymbol varName . Formula
                    $ Lambda [(map unTagFormula args, unTagFormula body)]
      Just (Formula (Lambda clauses@((prevArg,_):_))) -> do
          if length prevArg /= length args
            then do
             eqFail (Formula $ Variable varName) Err.def_diff_argcount
             return ()
            else updateSymbol varName . Formula . Lambda 
                            $ clauses ++ [(map unTagFormula args
                                          , unTagFormula body)]
          
      Just _ -> do
         eqFail (Formula $ Variable varName) $ Err.def_not_lambda varName
         return ()

-- | Add a "value" into the symbol table
addVar :: String -> Formula ListForm -> EqContext ()
addVar varName body = do
    symb <- symbolLookup varName
    case symb of
      Nothing -> addSymbol varName body
      Just _ -> do
         eqFail (Formula $ Variable varName) $ Err.def_already varName
         return ()

-- | Evaluate top level declarations
evalGlobalStatement :: EvalFun -> Formula ListForm -> EqContext (Formula ListForm)
evalGlobalStatement _ (Formula (BinOp OpAttrib [ (App (Variable funName) argList)
                                               , body ])) = do
    addLambda funName (map Formula argList) (Formula body)
    return $ Formula (BinOp OpEq [(App (Variable funName) argList), body])

evalGlobalStatement evaluator (Formula (BinOp OpAttrib [(Variable varName), body])) = do
    pushContext
    body' <- evaluator body
    popContext
    addVar varName (Formula body')
    return $ Formula (BinOp OpEq [(Variable varName), body'])

evalGlobalStatement evaluator (Formula e) = do
    pushContext
    a <- evaluator e
    popContext
    return $ Formula a

