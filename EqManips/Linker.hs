-- | This module will link variable names to
-- symbols.
module EqManips.Linker( DocString, LongDescr
                      , entityList
                      , metaFunctionList 
                      , unaryFunctions 
                      , multiParamsFunctions
                      , linkFormula
                      ) where

import EqManips.Types
import Data.List
import Data.Maybe( fromMaybe )
import qualified Data.Map as Map

-- | Linking formula doesn't change it's form,
-- so we can keep it
linkFormula :: Formula anyForm -> Formula anyForm
linkFormula (Formula a) = Formula $ link a

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
            , Complex (CInteger 0, CInteger 1)))
    ]

metaFunctionList :: [(String, (DocString, LongDescr, FormulaPrim -> FormulaPrim))]
metaFunctionList =
    [ ("Hold", ( "Avoid evaluating the expression passed as parameter."
               , ""
               , Meta Hold))
    , ("Force", ( "Force the evaluation of sub-expression even if the enclosing"
                , ""
                , Meta Force))
    , ("Expand", ( ""
                 , ""
                 , Meta Expand))
    , ("Cleanup", ( "Make trivial simplification to the formula"
                  , "Simplify things like '1 * x' to 'x'."
                  , Meta Cleanup))
    , ("Sort", ( ""
               , ""
               , Meta Sort))
    ]

unaryFunctions :: [(String, (DocString, LongDescr, FormulaPrim -> FormulaPrim))]
unaryFunctions =
    [ ("ceil", ( ""
               , ""
               , UnOp OpCeil))
    , ("floor", ( ""
                , ""
                , UnOp OpFloor))
    , ("frac", ( ""
               , ""
               , UnOp OpFrac))
    , ("sin", ( ""
              , ""
              , UnOp OpSin))
    , ("sinh", ( ""
               , ""
               , UnOp OpSinh))
    , ("asin", ( ""
               , ""
               , UnOp OpASin))
    , ("asinh", ( ""
                , ""
                , UnOp OpASinh))
    , ("cos", ( ""
              , ""
              , UnOp OpCos))
    , ("cosh", ( ""
               , ""
               , UnOp OpCosh))
    , ("acos", ( ""
               , ""
               , UnOp OpACos))
    , ("acosh", ( ""
                , ""
                , UnOp OpACosh))
    , ("tan", ( ""
              , ""
              , UnOp OpTan))
    , ("tanh", ( ""
               , ""
               , UnOp OpTanh))
    , ("atan", ( ""
               , ""
               , UnOp OpATan))
    , ("atanh", ( ""
                , ""
                , UnOp OpATanh))
    , ("abs", ( ""
              , ""
              , UnOp OpAbs))
    , ("sqrt", ( ""
               , ""
               , UnOp OpSqrt))
    , ("exp", ( ""
              , ""
              , UnOp OpExp))
    , ("log", ( ""
              , ""
              , UnOp OpLog))
    , ("ln", ( ""
             , ""
             , UnOp OpLn))
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

lambdaBuilder, derivateBuilder :: [FormulaPrim] -> FormulaPrim
lambdaBuilder [arg, body] = Meta LambdaBuild $ Lambda [([arg], body)]
lambdaBuilder lst = App (Variable "Lambda") lst

derivateBuilder [what, var] = Derivate what var
derivateBuilder lst = App (Variable "Derivate") lst


sumBuilder :: [FormulaPrim] -> FormulaPrim
sumBuilder [ini, end, what] = Sum ini end what
sumBuilder [ini, what] = Sum ini (Variable "") what
sumBuilder [what] = Sum (Variable "") (Variable "") what
sumBuilder lst = App (Variable "sum") lst

productBuilder :: [FormulaPrim] -> FormulaPrim
productBuilder [ini, end, what] = Product ini end what
productBuilder [ini, what] = Product ini (Variable "") what
productBuilder [what] = Product (Variable "") (Variable "") what
productBuilder lst = App (Variable "product") lst

integrateBuilder :: [FormulaPrim] -> FormulaPrim
integrateBuilder [ini, end, what, dvar] = Integrate ini end what dvar
integrateBuilder [ini, what, dvar] = Integrate ini (Variable "") what dvar
integrateBuilder [what, dvar] = Integrate (Variable "") (Variable "") what dvar
integrateBuilder lst = App (Variable "integrate") lst

matrixBuilder :: [FormulaPrim] -> FormulaPrim
matrixBuilder (CInteger n: CInteger m: exps)
    | fromEnum n * fromEnum m > length exps = error "The matrix has not enough expressions"
    | fromEnum n * fromEnum m < length exps = error "The matrix has too much expressions"
    | otherwise = Matrix (fromEnum n) (fromEnum m) $ splitMatrix exps
        where splitMatrix  [] = []
              splitMatrix lst =
                let (matrixLine, matrixRest) = genericSplitAt n lst
                in map link matrixLine : splitMatrix matrixRest
matrixBuilder lst = App (Variable "matrix") lst

-- | Function associating variables to symbol.
link :: FormulaPrim -> FormulaPrim
link (App (Variable "block") [CInteger i1, CInteger i2, CInteger i3]) = 
    Block (fromEnum i1) (fromEnum i2) (fromEnum i3)

-- Transformations for operators
link p@(Poly _) = p
link v@(Variable varName) =
    fromMaybe v $ Map.lookup varName entityTranslation
link (App (Variable funName) [x]) = 
    maybe (App (Variable funName) [linked]) (\f -> f linked)
    $ Map.lookup funName unaryTranslations
        where linked = link x

link (App (Variable v) flst) =
    maybe (App (Variable v) linked) (\f -> f linked) 
    $ Map.lookup v multiParametersFunction
        where linked = map link flst

-- General transformations
link (App f flst) = App (link f) $ map link flst
link (UnOp op f) = UnOp op $ link f
link (BinOp op fs) = BinOp op [link f | f <- fs]
link (Meta m fs) = Meta m $ link fs
link a@(CFloat _) = a
link a@(CInteger _) = a
link a@(NumEntity _) = a
link a@(Block _ _ _) = a
link t@(Truth _) = t
link f@(Fraction _) = f
link (Complex (r,i)) = Complex (link r, link i)
link (Lambda l) = Lambda [ (map link fl, link f)| (fl, f) <- l]
link (Matrix n m ll) = Matrix n m [map link rows | rows <- ll]
link (Derivate a b) = Derivate (link a) (link b)
link (Sum a b c) = Sum (link a) (link b) (link c)
link (Product a b c) = Sum (link a) (link b) (link c)
link (Integrate a b c d) = Integrate (link a) (link b) (link c) (link d)

