{-# LANGUAGE MagicHash #-}
module Monad.ListProducer( producedValue
                         , producedList
                         , runProducer
                         , ListProducer
                         , pushFront
                         , pushBack
                         , concatFront
                         , concatBack ) where
{-import GHC.Exts-}
{-import GHC.Prim-}
import Control.Monad.Fix

data ListProducer e a = ListProducer {
        runListProducer :: [[e]] -> ( [[e]], a)
    }

instance Functor (ListProducer e) where
    {-# INLINE fmap #-}
    fmap f prod =
        ListProducer $ \lst ->
            let (lst', val) = runListProducer prod lst
            in (lst', f val)
    
instance Monad (ListProducer e) where
    {-# INLINE return #-}
    return a = ListProducer $ \lst -> (lst, a)

    {-# INLINE (>>=) #-}
    m >>= k = ListProducer $ \lst ->
        let (lst', value) = runListProducer m lst
        in runListProducer (k value) $ lst'

instance MonadFix (ListProducer e) where
    mfix  f = ListProducer $ \lst -> 
        let (lst', val) = runListProducer (f val) lst 
        in (lst', val) 

producedValue :: ListProducer e a -> a
producedValue = snd . runProducer

producedList :: ListProducer e a -> [e]
producedList = fst . runProducer

runProducer :: ListProducer e a -> ([e], a)
runProducer producer =
    let (lst, val) = runListProducer producer $ []
    in (concat lst, val)

pushFront :: e -> ListProducer e ()
pushFront element =
    ListProducer $ \lst -> ([element] : lst, ())

pushBack :: e -> ListProducer e ()
pushBack element =
    ListProducer $ \lst -> (lst ++ [[element]], ())
    
concatFront :: [e] -> ListProducer e ()
concatFront toConcat =
    ListProducer $ \lst -> (toConcat : lst, ())

concatBack :: [e] -> ListProducer e ()
concatBack toConcat =
    ListProducer $ \lst -> (lst ++ [toConcat], ())

