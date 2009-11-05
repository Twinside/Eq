{-# OPTIONS_GHC -fno-warn-orphans #-}
module EqManips.Renderer.Cpp( convertToCpp, convertToCppS ) where

import Control.Monad.State.Lazy
import Control.Applicative
import Data.Maybe
import EqManips.Types
import EqManips.Algorithm.Utils
import qualified EqManips.ErrorMessages as Err

data CppConf = CppConf
    { failures :: [String]
    , nameCount :: Int
    }

type OutContext a = State CppConf a

convertToCpp :: Formula TreeForm -> String
convertToCpp f = convertToCppS f ""

convertToCppS :: Formula TreeForm -> ShowS
convertToCppS (Formula f) = fst $ runState (cNo f) defaultConf

instance Applicative (State s) where
    pure = return
    (<*>) = ap

defaultConf :: CppConf
defaultConf =
    CppConf { failures = []
            , nameCount = 0 }

stateUpdater :: (CppConf -> CppConf) -> OutContext ()
stateUpdater f = do
    context <- get
    put $ f context

genName :: OutContext Int
genName = do
    ctxt <- get
    let count = nameCount ctxt
    put $ ctxt { nameCount = count + 1 }
    return count

outFail :: String -> OutContext ShowS
outFail text = stateUpdater conser >> return id
    where conser ctxt = ctxt { failures = text : failures ctxt }

str :: String -> ShowS
str = (++)

char :: Char -> ShowS
char = (:)

cNo :: FormulaPrim -> OutContext ShowS
cNo = cOut Nothing

cppBinOps :: BinOperator -> ShowS
cppBinOps op = case lookup op localDef of
        Just s -> str (' ' : s ++ " ")
        Nothing -> str (' ' : binopString op ++ " ")
    where localDef = [ (OpAnd, "&&"), (OpOr, "||")
                     , (OpEq, "=="), (OpNe, "!=")
                     , (OpAttrib, "=")
                     ]

unOpEr :: UnOperator -> String
unOpEr OpNegate = "-"
unOpEr OpAbs =  "abs"
unOpEr OpSqrt =  "sqrt"
unOpEr OpLn = "log"
unOpEr OpLog = "log10"
unOpEr OpExp = "exp"
unOpEr OpSin =  "sin"
unOpEr OpCos =  "cos"
unOpEr OpTan = "tan"
unOpEr OpSinh = "sinh"
unOpEr OpCosh = "cosh"
unOpEr OpTanh = "tanh"
unOpEr OpASin = "asin"
unOpEr OpACos = "acos"
unOpEr OpATan = "atan"
unOpEr OpCeil = "ceil"
unOpEr OpFloor = "floor"
unOpEr OpFrac = ""
unOpEr OpFactorial = ""
unOpEr OpASinh = ""
unOpEr OpACosh = ""
unOpEr OpATanh = ""

cOut :: Maybe (BinOperator, Bool) -> FormulaPrim -> OutContext ShowS
cOut _ (CInteger i) = return $ shows i
cOut _ (CFloat i) = return $ shows i
cOut _ (Variable v) = return $ str v
cOut _ (Truth True) = return $ str "true"
cOut _ (Truth False) = return $ str "false"
cOut _ (NumEntity Pi) = return $ str "M_PI"
cOut _ (NumEntity _) = return $ str ""

cOut _ (App func args) =
    (\fun args' -> fun . char '(' . interspereseS (str ", ") args' . char ')')
    <$> cNo func 
    <*> mapM cNo args

cOut _ (UnOp op f) =
    (\sub -> str (unOpEr op) . char '(' . sub . char ')') <$> cNo f

cOut _ (BinOp OpAttrib [a,b]) =
    (\left right -> left . str " = " . right . str ";\n") <$> cNo a <*> cNo b

cOut _ (BinOp OpPow [a,b]) =
    (\left right -> str "pow( " . left . str ", " . right . str " ) ") <$> cNo a <*> cNo b

cOut Nothing (BinOp op [a,b]) = 
    (\left right -> left . cppBinOps op . right) <$> cOut (Just (op, False)) a 
                                       <*> cOut (Just (op, True)) b

cOut (Just (parent, right)) f@(BinOp op _)
    | needParenthesis right parent op = 
        (\sub -> char '(' . sub . char ')') <$> cNo f
    | otherwise = cOut Nothing f

cOut _ (BinOp _ []) = outFail $ Err.empty_binop "C output - "
cOut _ (BinOp _ [_]) = outFail $ Err.single_binop "C output - "
cOut _ (BinOp _ _) = outFail Err.c_out_bad_binop

cOut st (Meta _ f) = cOut st f
cOut _ (Sum begin ende what) = iteration "+" begin ende what
cOut _ (Product begin ende what) = iteration "*" begin ende what

cOut _ (Matrix _ _ _) = outFail Err.c_out_matrix
cOut _ (Derivate _ _) = outFail Err.c_out_derivate
cOut _ (Integrate _ _ _ _) = outFail Err.c_out_integrate
cOut _ (Lambda _) = outFail Err.c_out_lambda 
cOut _ (Block _ _ _) = outFail Err.c_out_block

iteration :: String -> FormulaPrim -> FormulaPrim -> FormulaPrim -> OutContext ShowS
iteration op (BinOp OpEq [Variable v, iniExpr]) exprEnd what = do
    tokenVar <- genName
    let tmpVar = "temp_" ++ show tokenVar
    initExpr <- cNo iniExpr
    exprEnd' <- cNo exprEnd
    whatExpr <- cNo what
    return $ str "double " . str tmpVar . str ";\n"
           . str "for ( int " . str v . str " = " . initExpr . str "; " 
                    . str v . str " < " . exprEnd' . str "; "
                    . str " )\n"
           . str "{\n"
           . str tmpVar . char ' ' . str op . str "= " . whatExpr . str ";\n"
           . str "}\n"
iteration _ _ _ _ = outFail Err.c_out_bad_iteration

