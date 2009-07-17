module EqManips.EvaluationContext where

newtype EqContext a = EqContext a

instance Functor EqContext where
    {-# INLINE fmap #-}
    fmap f (EqContext formula) = EqContext $ f formula
    
instance Monad EqContext where
    {-# INLINE return #-}
    return a = EqContext a

    {-# INLINE (>>=) #-}
    (EqContext a) >>= k = k a
        
