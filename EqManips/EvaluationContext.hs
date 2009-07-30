module EqManips.EvaluationContext( EqTransformInfo( .. )
                                 , EqContext
                                 , performTransformation 
                                 , addSymbol
                                 , eqFail
                                 , symbolLookup
                                 , pushContext, popContext
#ifdef _DEBUG
                                 , addTrace
                                 , printTrace
#endif /* _DEBUG */
                                 ) where

import EqManips.Types

#ifdef _DEBUG
import System.IO
import EqManips.Renderer.Ascii( formatFormula )
#endif /* _DEBUG */

-- | The real context info.
data EqTransformInfo = EqTransformInfo {
        -- | Well, here context mean more "symbol table"
        -- associate some variable with a definition.
          context    :: [(String, Formula)]
        -- | A context "stack" used to handle some scoping
        -- which can be used to evaluate some sums.
        , contextStack :: [[(String, Formula)]]

        -- | Some constraints put on variables
        , assertions :: [(String, Formula)]

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
        context = []
      , contextStack = []
      , assertions = []
      , errorList = []
      , result = Block 0 0 0
#ifdef _DEBUG
      , trace = []
#endif /* _DEBUG */
    }

#ifdef _DEBUG
addTrace :: (String,Formula) -> EqContext ()
addTrace newTrace = EqContext $ \c ->
    (c { trace = newTrace : trace c }, ())

printTrace :: Handle -> EqTransformInfo -> IO ()
printTrace f inf = mapM_ showIt $ trace inf
    where showIt (str, formula) = do
              hPutStrLn f "=========================================="
              hPutStrLn f str
              hPutStrLn f $ formatFormula formula
#endif /* _DEBUG */

-- | Keep a track of current context, keep previous context clean
pushContext :: EqContext ()
pushContext = EqContext $ \c ->
    (c { contextStack = context c : contextStack c }, ())

-- | Discard the current deep context and restore the one
-- which was previously "pushed" by pushContext. If no
-- context was there, an empty one is put in place
popContext :: EqContext ()
popContext = EqContext $ \c ->
    let safeHeadTail (x:xs) = (x, xs)
        safeHeadTail     [] = ([], [])
        (oldContext, stack) = safeHeadTail $ contextStack c
    in
    (c { contextStack = stack, context = oldContext }, ())

-- | Public function of the API to retrieve the result of
-- a formula transformation. The type is opaque otherwise.
performTransformation :: EqContext Formula -> EqTransformInfo
performTransformation m = ctxt { result = formula }
    where (ctxt, formula) = runEqTransform m emptyContext

-- | Add a variable into the context
addSymbol :: String -> Formula -> EqContext ()
addSymbol varName def = EqContext $ \eqCtxt ->
    let prevSymbol = context eqCtxt
    in ( eqCtxt{ context = (varName,def): prevSymbol }, ())

symbolLookup :: String -> EqContext (Maybe Formula)
symbolLookup varName = EqContext $ \eqCtxt ->
    (eqCtxt, lookup varName $ context eqCtxt)

-- | Used to provide error messages at the end of the computation
-- (when jumping back to IO), and also assure a nice partial evaluation,
-- by replacing the faulty formula by a block.
eqFail :: Formula -> String -> EqContext Formula
eqFail formula errorText = EqContext $ \eqCtxt ->
    let prevErr = errorList eqCtxt
    in ( eqCtxt {errorList = (formula, errorText):prevErr}, Block 1 1 1)

