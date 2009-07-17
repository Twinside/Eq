-- | Great number mascarade in Haskell.
-- This module implement the type Aproximated which
-- got an instance for all the numeric types specified
-- in the haskell report.
--
-- The Aproximated types embed a real number to perform
-- real computations and in the same time, record a formula
-- tracing what happened to the number.
--
-- After the end of the computation, you can retrieve the
-- formula and display it for debugging or code documentation.
--
-- You may want to perform symbolic computation on the resulting 
-- formula.
module EqManips.Aproximated( Aproximated
                           , formulaOfAproximation 
                           , valueOfAproximation 
                           ) where

import EqManips.Types

newtype Aproximated a =
    Aproximated (Formula, a)

formulaOfAproximation :: Aproximated a -> Formula
formulaOfAproximation (Aproximated (f, _)) = f

valueOfAproximation :: Aproximated a -> a
valueOfAproximation (Aproximated (_, n)) = n

-- | In this instance, we provide comparaison result
-- on the embedded number, to avoid altering it's
-- semantic.
instance (Eq a) => Eq (Aproximated a) where
    (Aproximated (_,n)) == (Aproximated (_,n2)) =
        n == n2
    (Aproximated (_,n)) /= (Aproximated (_,n2)) =
        n /= n2

instance (Ord a) => Ord (Aproximated a) where
    (Aproximated (_,n)) < (Aproximated (_,n2)) =
        n < n2
    (Aproximated (_,n)) >= (Aproximated (_,n2)) =
        n >= n2
    (Aproximated (_,n)) > (Aproximated (_,n2)) =
        n > n2
    (Aproximated (_,n)) <= (Aproximated (_,n2)) =
        n <= n2

-- | Not a really useful declaration, but needed
-- to implement Num, so here we go...
instance (Show a) => Show (Aproximated a) where
    show (Aproximated (_,n)) = show n 
    
instance (Show a, Num a) => Num (Aproximated a) where
    (Aproximated (fa, na)) + (Aproximated (fb, nb)) =
        Aproximated (fa + fb, na + nb)
    (Aproximated (fa, na)) - (Aproximated (fb, nb)) =
        Aproximated (fa - fb, na - nb)
    (Aproximated (fa, na)) * (Aproximated (fb, nb)) =
        Aproximated (fa * fb, na * nb)
    negate (Aproximated (f,n)) = 
        Aproximated (negate f, negate n)
    abs (Aproximated (f,n)) = 
        Aproximated (abs f, abs n)
    signum (Aproximated (f,n)) =
        Aproximated (signum f, signum n)
    fromInteger i = 
        Aproximated (fromInteger i, fromInteger i)

instance (Real a) => Real (Aproximated a) where
    toRational (Aproximated (_,n)) = toRational n
    
instance (Fractional a) => Fractional (Aproximated a) where
    (Aproximated (fa, na)) / (Aproximated (fb, nb)) =
        Aproximated (fa / fb, na / nb)
    recip (Aproximated (f,n)) = Aproximated (recip f, recip n)
    fromRational i = 
        Aproximated (fromRational i, fromRational i)

instance (Enum a) => Enum (Aproximated a) where
    fromEnum (Aproximated (_,n)) = fromEnum n
    toEnum i =
        Aproximated ( CInteger i, toEnum i)

    {- TODO 1 maybe define a succ function in formula -}
    succ (Aproximated (f,n)) =
        Aproximated (f + CInteger 1, succ n)

    {- TODO 1 maybe define a pred function in formula -}
    pred (Aproximated (f,n)) =
        Aproximated (f - CInteger 1, pred n)
    
instance Integral a => Integral (Aproximated a) where
    {-TODO 4 Specify quot in formula-}
    quot (Aproximated (f1, n1)) (Aproximated (f2, n2)) =
        Aproximated (f1 / f2, n1 `quot` n2)

    {-TODO 4 Specify rem in formula-}
    rem (Aproximated (f1, n1)) (Aproximated (f2, n2)) =
        Aproximated (f1 / f2, n1 `rem` n2)

    {-TODO 4 specify div in formula-}
    div (Aproximated (f1, n1)) (Aproximated (f2, n2)) =
        Aproximated (f1 / f2, n1 `div` n2)

    {-TODO 4 specify mod in formula-}
    mod (Aproximated (f1, n1)) (Aproximated (f2, n2)) =
        Aproximated (f1 / f2, n1 `mod` n2)

    quotRem n1 n2 = (n1 `quot` n2, n1 `rem`n2)
    divMod n1 n2 = (n1 `div` n2, n1 `mod` n2)

    toInteger (Aproximated (_,n)) = toInteger n

instance RealFrac a => RealFrac (Aproximated a) where
    {- TODO 3 Maybe add frac as an operator 
     - somehting -}
    properFraction (Aproximated (f,n)) = 
        let (integ, frac) = properFraction n 
            app = App (Variable "frac") [f]
        in ( integ
           , Aproximated (app ,frac))

    {- TODO 3 add truncation to formula -}
    truncate (Aproximated (f,n)) =
        let newNum :: (Integral b) => b
            newNum = truncate n
        in Aproximated (f, newNum)

    {- TODO 3 add rounding to formula -}
    round (Aproximated (f,n)) = 
        Aproximated (f, round n)

    {- TODO 3 add ceiling to formula -}
    ceiling (Aproximated (f,n)) = 
        Aproximated (f, ceiling n)

    {- TODO 3 add floor to formula -}
    floor (Aproximated (f,n)) =
        Aproximated (f, floor n)

