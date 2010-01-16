module EqManips.EvaluationContext( EqTransformInfo( .. )
                                 , EqContext
                                 , performTransformation 
                                 , performTransformationWithContext
                                 , performLastTransformation 
                                 , performLastTransformationWithContext 
                                 , obtainEqResult 
                                 , cleanErrorList 
                                 , addSymbols 
                                 , addSymbol, delSymbol, updateSymbol 
                                 , eqFail, eqPrimFail 
                                 , symbolLookup
                                 , pushContext, popContext, setContext 
                                 , contextStackSize 
#ifdef _DEBUG
                                 , addTrace
                                 , printTrace
                                 , traceContext 
#endif /* _DEBUG */
                                 ) where

import Data.List
import Data.Map (Map)
import Control.Applicative
import qualified Data.Map as Map

import EqManips.Types
import EqManips.Algorithm.Utils

#ifdef _DEBUG
import System.IO
import EqManips.Renderer.Ascii( formatFormula )
import EqManips.Renderer.Sexpr
#endif /* _DEBUG */

-- | The real context info.
data EqTransformInfo = EqTransformInfo {
        -- | Well, here context mean more "symbol table"
        -- associate some variable with a definition.
          context    :: Map String (Formula ListForm)
        -- | A context "stack" used to handle some scoping
        -- which can be used to evaluate some sums.
        , contextStack :: [Map String (Formula ListForm)]

        -- | Depth of the context stack. Used to limit
        -- recursion in the monad.
        , contextDepth :: !Int

        -- | Some constraints put on variables
        , assertions :: Map String FormulaPrim

        -- | List of errors encountered when
        -- transforming formula
        , errorList  :: [(Formula TreeForm,String)]

        -- | The result of the formula computation
        , result :: Formula ListForm

#ifdef _DEBUG
        -- | Used for debugging, can print everything
        , trace :: [(String, Formula TreeForm)]
#endif /* _DEBUG */
    }

-- | Here we go, our evaluation monad.
-- It's basically a State monad, but providing
-- more services usefull to the software
data EqContext a = EqContext {
        runEqTransform :: EqTransformInfo -> (EqTransformInfo, a)
    }

instance Functor EqContext where
    {-# INLINE fmap #-}
    fmap f m = EqContext $ \c ->
        let (c', a) = runEqTransform m c
        in (c', f a)

instance Applicative EqContext where
    pure a = EqContext $ \c -> (c,a) 
    (EqContext ff) <*> (EqContext a) = EqContext $ \c ->
        let (c' , f) = ff c
            (c'', a') = a c'
        in (c'', f a')

instance Monad EqContext where
    {-# INLINE return #-}
    return a = EqContext $ \c -> (c, a)

    {-# INLINE (>>=) #-}
    prev >>= k = EqContext $ \c -> 
        let (c', a) = runEqTransform prev c
        in runEqTransform (k a) c'

-- | A basic initial context
emptyContext :: EqTransformInfo 
emptyContext = EqTransformInfo {
        context = Map.empty
      , contextStack = []
      , contextDepth = 0
      , assertions = Map.empty
      , errorList = []
      , result = Formula $ Block 0 0 0
#ifdef _DEBUG
      , trace = []
#endif /* _DEBUG */
    }

#ifdef _DEBUG
-- | Function used to add a trace in debug.
-- don't forget to surround it's use by #ifdef _DEBUG/#endif
addTrace :: (String, Formula TreeForm) -> EqContext ()
addTrace newTrace = EqContext $ \c ->
    (c { trace = newTrace : trace c }, ())

-- | Print all the trace found.
printTrace :: Handle -> EqTransformInfo -> IO ()
printTrace f inf = mapM_ showIt . reverse $ trace inf
    where showIt (str, formula) = do
              hPutStrLn f "=========================================="
              hPutStrLn f str
              hPutStrLn f $ sexprRender formula
              hPutStrLn f $ formatFormula formula

traceContext :: EqContext ()
traceContext = EqContext $ \c ->
    let contextes = unlines 
                  . map (\a -> printContext a ++ "\n/////////////////////////////////////////////////\n") 
                  . map Map.toList
                  $ contextStack c
        printContext var = concat $ map (\(a,f) -> a ++ " =\n" 
                                                ++ formatFormula (treeIfyFormula f)
                                                ++ "\n")
                                        var
    in
    ( c { trace = ("ContextStack | " ++ contextes, Formula $ Variable "")
                : ("Context | " ++ (show $ context c), Formula $ Variable "") : trace c }
    , ()
    )
#endif /* _DEBUG */

-- | Keep a track of current context, keep previous context clean
pushContext :: EqContext ()
pushContext = EqContext $ \c ->
    (c { contextStack = context c : contextStack c
       , contextDepth = contextDepth c + 1
       }
    , ())

-- | Discard the current deep context and restore the one
-- which was previously "pushed" by pushContext. If no
-- context was there, an empty one is put in place
popContext :: EqContext ()
popContext = EqContext $ \c ->
    let safeHeadTail (x:xs) = (x, xs)
        safeHeadTail     [] = (Map.empty, [])
        (oldContext, stack) = safeHeadTail $ contextStack c
    in
    (c { contextStack = stack
       , context = oldContext
       , contextDepth = contextDepth c - 1
       }
    , ())

setContext :: [(String, Formula ListForm)] -> EqContext ()
setContext newContext = EqContext $ \c ->
    (c { context = Map.fromList newContext }, ())

-- | Cleanup error list, useful in cases of
-- threaded computation
cleanErrorList :: EqContext ()
cleanErrorList = EqContext $ \c -> (c { errorList = [] }, ())

type FormulaForm = ListForm

-- | Public function of the API to retrieve the result of
-- a formula transformation. The type is opaque otherwise.
performTransformation :: EqContext (Formula FormulaForm) -> EqTransformInfo
performTransformation = performTransformationWithContext Map.empty

-- | Evaluate a formula, you can provide variable bindings
performTransformationWithContext :: Map String (Formula ListForm)
                                 -> EqContext (Formula ListForm)
								 -> EqTransformInfo
performTransformationWithContext base m = ctxt { result = formula }
    where (ctxt, formula) = runEqTransform m $ emptyContext { context = base }

-- | Evaluate a programm, with no pre-definitions
performLastTransformation :: EqContext [Formula FormulaForm] -> EqTransformInfo
performLastTransformation =
	performLastTransformationWithContext Map.empty

-- | Run a programm and get the last statement.
-- You can run programm with your pre-defined symbols
performLastTransformationWithContext :: Map String (Formula ListForm)
                                     -> EqContext [Formula FormulaForm]
									 -> EqTransformInfo
performLastTransformationWithContext c m = ctxt { result = last formula }
    where (ctxt, formula) = runEqTransform m $ emptyContext { context = c }

obtainEqResult :: EqContext a -> a
obtainEqResult m = snd $ runEqTransform m emptyContext

-- | Remove a variable from the context
delSymbol :: String -> EqContext ()
delSymbol s = EqContext $ \ctxt ->
    (ctxt { context = Map.delete s $ context ctxt}, ())

updateSymbol :: String -> Formula ListForm -> EqContext ()
updateSymbol varName def = do
    delSymbol varName
    addSymbol varName def

addSymbols :: [(String, Formula ListForm)] -> EqContext ()
addSymbols adds = EqContext $ \eqCtxt ->
    let syms = context eqCtxt
    in -- union is left biased, we use it here, new symbols
       -- at the left of union !!
    ( eqCtxt { context = Map.fromList adds `Map.union` syms}, ())

-- | Add a variable into the context
addSymbol :: String -> Formula ListForm -> EqContext ()
addSymbol varName def = EqContext $ \eqCtxt ->
    let prevSymbol = context eqCtxt
    in ( eqCtxt{ context = Map.insert varName def prevSymbol }, ())

contextStackSize :: EqContext Int
contextStackSize = EqContext $ \eqCtxt ->
    (eqCtxt, contextDepth eqCtxt)

-- | Check if a symbol is present, and if so, return it's
-- definition
symbolLookup :: String -> EqContext (Maybe (Formula ListForm))
symbolLookup varName = EqContext $ \eqCtxt ->
    (eqCtxt, Map.lookup varName $ context eqCtxt)

-- | Used to provide error messages at the end of the computation
-- (when jumping back to IO), and also assure a nice partial evaluation,
-- by replacing the faulty formula by a block.
eqFail :: Formula TreeForm -> String -> EqContext (Formula a)
eqFail formula errorText = EqContext $ \eqCtxt ->
    let prevErr = errorList eqCtxt
    in ( eqCtxt {errorList = (formula, errorText):prevErr}, Formula $ Block 1 1 1)

-- | Little helper to be able to use eqFail easily when
-- manipulating FormulaPrim formula. Assume that FormulaPrim
-- is in List Form. Use eqFail otherwise.
eqPrimFail :: FormulaPrim -> String -> EqContext FormulaPrim
eqPrimFail f s = unTagFormula `fmap` eqFail (treeIfyFormula $ Formula f) s

