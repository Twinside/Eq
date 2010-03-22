-- | This module will link variable names to
-- symbols.
module EqManips.Linker( DocString, LongDescr
                      , entityList
                      , metaFunctionList 
                      , unaryFunctions 
                      , multiParamsFunctions
                      , linkFormula
                      ) where

import Control.Applicative
import Data.List
import Data.Maybe( fromMaybe )
import qualified Data.Map as Map

import EqManips.Types
import EqManips.EvaluationContext

-- | Linking formula doesn't change it's form,
-- so we can keep it
linkFormula :: Formula anyForm -> EqContext (Formula anyForm)
linkFormula (Formula a) = Formula <$> link a

type DocString = String
type LongDescr = String

entityList :: [(String, (DocString, LongDescr, FormulaPrim))]
entityList =
    [ ("infinite", ("Represent the inifinity in this program."
                   , ""
                   , NumEntity Infinite))
    , ("pi", ( "The number Pi (=3.14159...)."
             , "When used, exact simplification can be used"
             , NumEntity Pi))
    , ("i", ( "The imaginary number, use it to describe complex numbers."
            , "i * i = -1"
            , complex (CInteger 0, CInteger 1)))
    ]

metaFunctionList :: [(String, (DocString, LongDescr, FormulaPrim -> FormulaPrim))]
metaFunctionList =
    [ ("Hold", ( "Avoid evaluating the expression passed as parameter."
               , ""
               , meta Hold))
    , ("Force", ( "Force the evaluation of sub-expression even if the enclosing"
                , ""
                , meta Force))
    , ("Expand", ( ""
                 , ""
                 , meta Expand))
    , ("Cleanup", ( "Make trivial simplification to the formula"
                  , "Simplify things like '1 * x' to 'x'."
                  , meta Cleanup))
    , ("Sort", ( ""
               , ""
               , meta Sort))
    ]

unaryFunctions :: [(String, (DocString, LongDescr, FormulaPrim -> FormulaPrim))]
unaryFunctions =
    [ ("ceil", ( ""
               , ""
               , unOp OpCeil))
    , ("floor", ( ""
                , ""
                , unOp OpFloor))
    , ("frac", ( ""
               , ""
               , unOp OpFrac))
    , ("sin", ( ""
              , ""
              , unOp OpSin))
    , ("sinh", ( ""
               , ""
               , unOp OpSinh))
    , ("asin", ( ""
               , ""
               , unOp OpASin))
    , ("asinh", ( ""
                , ""
                , unOp OpASinh))
    , ("cos", ( ""
              , ""
              , unOp OpCos))
    , ("cosh", ( ""
               , ""
               , unOp OpCosh))
    , ("acos", ( ""
               , ""
               , unOp OpACos))
    , ("acosh", ( ""
                , ""
                , unOp OpACosh))
    , ("tan", ( ""
              , ""
              , unOp OpTan))
    , ("tanh", ( ""
               , ""
               , unOp OpTanh))
    , ("atan", ( ""
               , ""
               , unOp OpATan))
    , ("atanh", ( ""
                , ""
                , unOp OpATanh))
    , ("abs", ( ""
              , ""
              , unOp OpAbs))
    , ("sqrt", ( ""
               , ""
               , unOp OpSqrt))
    , ("exp", ( ""
              , ""
              , unOp OpExp))
    , ("log", ( ""
              , ""
              , unOp OpLog))
    , ("ln", ( ""
             , ""
             , unOp OpLn))
    ]

unaryTranslations :: Map.Map String (FormulaPrim -> FormulaPrim)
unaryTranslations = Map.fromList
    [ (name, fun) | (name, (_,_,fun)) <- unaryFunctions ++ metaFunctionList ]

entityTranslation :: Map.Map String FormulaPrim
entityTranslation = Map.fromList [(name, val) | (name, (_,_,val)) <- entityList]

multiParametersFunction :: Map.Map String ([FormulaPrim] -> FormulaPrim)
multiParametersFunction = Map.fromList [(name, f) | (name, (_,_,_,f)) <- multiParamsFunctions]

multiParamsFunctions :: [ ( String
                          , (DocString, LongDescr, [(DocString,LongDescr)], [FormulaPrim] -> FormulaPrim))]
multiParamsFunctions =
    [ ("Lambda", ( "Create an anonymous function"
                 , "An anonymous function is a function with no name which can be passed as parameter."
                 , [ ("Argument", "Variable to be bound when the lambda is called")
                   , ("Body", "Expression to be evaluated after argument binding.\n"
                            ++"The body is not evaluated during it's definition.")
                   ]
                 , lambdaBuilder )  )
    , ("derivate", ( "Make a partial differentiation"
                   , "Differentiate an expression for a variable given in parameter."
                   , [ ("Expression", "Expression to be differentiated, no evaluation occur at binding, unless it is in Force()")
                     , ("Variable", "Variable on which to perform partial differentiation. No evaluation done unless in Force()")
                     ]
                   , derivateBuilder
                   ))

    , ("sum", ( "Perform a sum of an expression"
              , "The sum bind a variable over a range and perform a sum. If the arguments below are not given, no calculation is performed."
              , [ ("Initial value", "An expression in the form x = something, to declare the start of iteration.")
                , ("End value", "An upper bound for iteration, must be a number for calculation to happen")
                , ("Expression", "Expression to be summed, can contain the variable bound by initial value.")
                ]
              , sumBuilder))
    , ("product", ( "Perform a product of an expression"
                , "The sum bind a variable over a range and perform a sum. If the arguments below are not given, no calculation is performed."
                , [ ("Initial value", "An expression in the form x = something, to declare the start of iteration.")
                  , ("End value", "An upper bound for iteration, must be a number for calculation to happen")
                  , ("Expression", "Expression to be summed, can contain the variable bound by initial value.")
                  ]
                , productBuilder ))
    , ("integrate", ( "Describe an integral"
                    , "For the moment, no calculation is performed. Just used for the format command"
                    , [ ("Initial Value", "Lower bound of the integral.")
                      , ("End Value", "Upper bound of the integral.")
                      , ("Expression", "The expression to be integrated.")
                      , ("Variable", "The dx of the integral, where x is any variable.")
                      ]
                    , integrateBuilder))
    , ("matrix", ( "Create a matrix"
                 , ""
                 , [("width", "Number of columns")
                   ,("height", "Number of lines of the matrix")
                   ,("...", "All the values")
                   ]
                 , matrixBuilder ))
    ]

lambdaBuilder :: [FormulaPrim] -> FormulaPrim
lambdaBuilder [arg, body] = meta LambdaBuild $ lambda [([arg], body)]
lambdaBuilder lst = app (Variable "Lambda") lst

derivateBuilder :: [FormulaPrim] -> FormulaPrim
derivateBuilder [what, var] = derivate what var
derivateBuilder lst = app (Variable "Derivate") lst


sumBuilder :: [FormulaPrim] -> FormulaPrim
sumBuilder [ini, end, what] = summ ini end what
sumBuilder [ini, what] = summ ini (Variable "") what
sumBuilder [what] = summ (Variable "") (Variable "") what
sumBuilder lst = app (Variable "sum") lst

productBuilder :: [FormulaPrim] -> FormulaPrim
productBuilder [ini, end, what] = productt ini end what
productBuilder [ini, what] = productt ini (Variable "") what
productBuilder [what] = productt (Variable "") (Variable "") what
productBuilder lst = app (Variable "product") lst

integrateBuilder :: [FormulaPrim] -> FormulaPrim
integrateBuilder [ini, end, what, dvar] = integrate ini end what dvar
integrateBuilder [ini, what, dvar] = integrate ini (Variable "") what dvar
integrateBuilder [what, dvar] = integrate (Variable "") (Variable "") what dvar
integrateBuilder lst = app (Variable "integrate") lst

matrixBuilder :: [FormulaPrim] -> FormulaPrim
matrixBuilder (CInteger n: CInteger m: exps)
    | fromEnum n * fromEnum m > length exps = error "The matrix has not enough expressions"
    | fromEnum n * fromEnum m < length exps = error "The matrix has too much expressions"
    | otherwise = matrix (fromEnum n) (fromEnum m) $ splitMatrix exps
        where splitMatrix  [] = []
              splitMatrix lst =
                let (matrixLine, matrixRest) = genericSplitAt n lst
                in matrixLine : splitMatrix matrixRest
matrixBuilder lst = app (Variable "matrix") lst

multivarLinker :: String -> [FormulaPrim] -> EqContext FormulaPrim
multivarLinker v flst =
    maybe (app (Variable v) <$> linked) (\f -> f <$> linked) 
    $ Map.lookup v multiParametersFunction
        where linked = mapM link flst

-- | Function associating variables to symbol.
link :: FormulaPrim -> EqContext FormulaPrim
link (App _ (Variable "block") [CInteger i1, CInteger i2, CInteger i3]) = 
    pure $ Block (fromEnum i1) (fromEnum i2) (fromEnum i3)

-- Transformations for operators
link p@(Poly _ _) = pure p
link v@(Variable varName) =
    pure $ fromMaybe v $ Map.lookup varName entityTranslation
link (App _ (Variable funName) [x]) = 
      maybe (multivarLinker funName [x]) (\f -> f <$> linked)
    $ Map.lookup funName unaryTranslations
        where linked = link x

link (App _ (Variable v) flst) = multivarLinker v flst

-- General transformations
link (App _ f flst) = app <$> (link f) <*> mapM link flst
link (UnOp _ op f) = unOp op <$> link f
link (BinOp _ op fs) = binOp op <$> mapM link fs
link (Meta _ m fs) = meta m <$> link fs
link a@(CFloat _) = pure a
link a@(CInteger _) = pure a
link a@(NumEntity _) = pure a
link a@(Block _ _ _) = pure a
link t@(Truth _) = pure t
link f@(Fraction _) = pure f
link (Complex _ (r,i)) = (\r i -> complex (r,i)) <$> link r <*> link i
link (Lambda _ l) = lambda <$> sequence [ (,) <$> mapM link fl <*> link f | (fl, f) <- l]
link (Matrix _ n m ll) = matrix n m <$> sequence [mapM link rows | rows <- ll]
link (Derivate _ a b) = derivate <$> link a <*> link b
link (Sum _ a b c) = summ <$> link a <*> link b <*> link c
link (Product _ a b c) = productt <$> link a <*> link b <*> link c
link (Integrate _ a b c d) = integrate <$> link a <*> link b <*> link c <*> link d

