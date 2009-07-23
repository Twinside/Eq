module EqManips.EvaluationContext( EqTransformInfo( .. )
                                 , EqContext
                                 , performTransformation 
                                 , addSymbol
                                 , eqFail
                                 , symbolLookup
                                 ) where

import EqManips.Types

data EqTransformInfo = EqTransformInfo {
          context    :: [(String, Formula)]
        , assertions :: [(String, Formula)]
        , errorList  :: [(Formula,String)]
        , result :: Formula
    }

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

emptyContext :: EqTransformInfo 
emptyContext = EqTransformInfo {
        context = []
      , assertions = []
      , errorList = []
      , result = Block 0 0 0
    }

performTransformation :: EqContext Formula -> EqTransformInfo
performTransformation m = ctxt { result = formula }
    where (ctxt, formula) = runEqTransform m emptyContext

addSymbol :: String -> Formula -> EqContext ()
addSymbol varName def = EqContext $ \eqCtxt ->
    let prevSymbol = context eqCtxt
    in ( eqCtxt{ context = (varName,def): prevSymbol }, ())

eqFail :: Formula -> String -> EqContext Formula
eqFail formula errorText = EqContext $ \eqCtxt ->
    let prevErr = errorList eqCtxt
    in ( eqCtxt {errorList = (formula, errorText):prevErr}, Block 1 2 2)

symbolLookup :: String -> EqContext (Maybe Formula)
symbolLookup varName = EqContext $ \eqCtxt ->
    (eqCtxt, lookup varName $ context eqCtxt)
