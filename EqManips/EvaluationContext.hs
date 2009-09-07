module EqManips.EvaluationContext( EqTransformInfo( .. )
                                 , EqContext
                                 , performTransformation 
                                 , performLastTransformation 
                                 , obtainEqResult 
                                 , cleanErrorList 
                                 , addSymbols 
                                 , addSymbol, delSymbol, updateSymbol 
                                 , eqFail
                                 , symbolLookup
                                 , pushContext, popContext, setContext 
#ifdef _DEBUG
                                 , addTrace
                                 , printTrace
                                 , traceContext 
#endif /* _DEBUG */
                                 ) where

import EqManips.Types
import Data.List
import Control.Applicative

import Data.Map (Map)
import qualified Data.Map as Map

#ifdef _DEBUG
import System.IO
import EqManips.Renderer.Ascii( formatFormula )
#endif /* _DEBUG */

-- | The real context info.
data EqTransformInfo = EqTransformInfo {
        -- | Well, here context mean more "symbol table"
        -- associate some variable with a definition.
          context    :: Map String Formula
        -- | A context "stack" used to handle some scoping
        -- which can be used to evaluate some sums.
        , contextStack :: [Map String Formula]

        -- | Depth of the context stack. Used to limit
        -- recursion in the monad.
        , contextDepth :: !Int

        -- | Some constraints put on variables
        , assertions :: Map String Formula

        -- | List of errors encountered when
        -- transforming formula
        , errorList  :: [(Formula,String)]

        -- | The result of the formula computation
        , result :: Formula

#ifdef _DEBUG
        -- | Used for debugging, can print everything
        , trace :: [(String, Formula)]
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
        in runEqTransform (k a) $ c'

-- | A basic initial context
emptyContext :: EqTransformInfo 
emptyContext = EqTransformInfo {
        context = Map.empty
      , contextStack = []
      , contextDepth = 0
      , assertions = Map.empty
      , errorList = []
      , result = Block 0 0 0
#ifdef _DEBUG
      , trace = []
#endif /* _DEBUG */
    }

#ifdef _DEBUG
-- | Function used to add a trace in debug.
-- don't forget to surround it's use by #ifdef _DEBUG/#endif
addTrace :: (String,Formula) -> EqContext ()
addTrace newTrace = EqContext $ \c ->
    (c { trace = newTrace : trace c }, ())

-- | Print all the trace found.
printTrace :: Handle -> EqTransformInfo -> IO ()
printTrace f inf = mapM_ showIt . reverse $ trace inf
    where showIt (str, formula) = do
              hPutStrLn f "=========================================="
              hPutStrLn f str
              hPutStrLn f $ formatFormula formula

traceContext :: EqContext ()
traceContext = EqContext $ \c ->
    let contextes = unlines 
                  . map (\a -> printContext a ++ "\n/////////////////////////////////////////////////\n") 
                  . map Map.toList
                  $ contextStack c
        printContext var = concat $ map (\(a,f) -> a ++ " =\n" ++ formatFormula f ++ "\n") var
    in
    (c { trace = ("ContextStack | " ++ contextes, Variable ""): ("Context | " ++ (show $ context c), Variable "") : trace c }, ())
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

setContext :: [(String, Formula)] -> EqContext ()
setContext newContext = EqContext $ \c ->
    (c { context = Map.fromList newContext }, ())

-- | Cleanup error list, useful in cases of
-- threaded computation
cleanErrorList :: EqContext ()
cleanErrorList = EqContext $ \c -> (c { errorList = [] }, ())

-- | Public function of the API to retrieve the result of
-- a formula transformation. The type is opaque otherwise.
performTransformation :: EqContext Formula -> EqTransformInfo
performTransformation m = ctxt { result = formula }
    where (ctxt, formula) = runEqTransform m emptyContext

performLastTransformation :: EqContext [Formula] -> EqTransformInfo
performLastTransformation m = ctxt { result = last formula }
    where (ctxt, formula) = runEqTransform m emptyContext

obtainEqResult :: EqContext a -> a
obtainEqResult m = snd $ runEqTransform m emptyContext

-- | Remove a variable from the context
delSymbol :: String -> EqContext ()
delSymbol s = EqContext $ \ctxt ->
    (ctxt { context = Map.delete s $ context ctxt}, ())

updateSymbol :: String -> Formula -> EqContext ()
updateSymbol varName def = do
    delSymbol varName
    addSymbol varName def

addSymbols :: [(String, Formula)] -> EqContext ()
addSymbols adds = EqContext $ \eqCtxt ->
    let syms = context eqCtxt
    in -- union is left biased, we use it here, new symbols
       -- at the left of union !!
    ( eqCtxt { context = Map.fromList adds `Map.union` syms}, ())

-- | Add a variable into the context
addSymbol :: String -> Formula -> EqContext ()
addSymbol varName def = EqContext $ \eqCtxt ->
    let prevSymbol = context eqCtxt
    in ( eqCtxt{ context = Map.insert varName def prevSymbol }, ())

-- | Check if a symbol is present, and if so, return it's
-- definition
symbolLookup :: String -> EqContext (Maybe Formula)
symbolLookup varName = EqContext $ \eqCtxt ->
    (eqCtxt, Map.lookup varName $ context eqCtxt)

-- | Used to provide error messages at the end of the computation
-- (when jumping back to IO), and also assure a nice partial evaluation,
-- by replacing the faulty formula by a block.
eqFail :: Formula -> String -> EqContext Formula
eqFail formula errorText = EqContext $ \eqCtxt ->
    let prevErr = errorList eqCtxt
    in ( eqCtxt {errorList = (formula, errorText):prevErr}, Block 1 1 1)

