module Language.Eq.Domain where

-- | Describe the bound kinds of an interval
data Openness =
    Include     -- ^ [0;1] 0 and 1 included
  | Exclude     -- ^ ]0;1[ 0 and 1 excluded
  deriving (Eq, Show)

data Limit =
      PlusInf
    | MinusInf
    | Val Double
    deriving (Eq, Show)

instance Ord Limit where
    compare PlusInf PlusInf = EQ
    compare MinusInf MinusInf = EQ
    compare (Val a) (Val b) = compare a b

    compare PlusInf MinusInf = GT
    compare MinusInf PlusInf = LT

    compare PlusInf (Val _) = GT
    compare MinusInf (Val _) = LT
    compare (Val _) PlusInf = LT
    compare (Val _) MinusInf = GT

instance Num Limit where
    --------------------------------------------------
    ---- '*' :: a -> a -> a (or error :p)
    --------------------------------------------------
    (Val a) + (Val b) = Val $ a + b

    PlusInf + PlusInf = PlusInf
    PlusInf + (Val _) = PlusInf
    (Val _) + PlusInf = PlusInf

    MinusInf + MinusInf = MinusInf
    MinusInf + (Val  _) = MinusInf
    (Val  _) + MinusInf = MinusInf

    MinusInf + PlusInf  = error "Infinite addition"
    PlusInf  + MinusInf = error "Infinite addition"

    --------------------------------------------------
    ---- '*' :: a -> a -> a
    --------------------------------------------------
    (Val a) * (Val b) = Val $ a * b

    PlusInf  * PlusInf = PlusInf
    MinusInf * MinusInf = PlusInf
    PlusInf  * MinusInf = MinusInf
    MinusInf * PlusInf  = MinusInf

    PlusInf * a@(Val _) = a * PlusInf
    (Val a) * PlusInf | a > 0 = PlusInf
                      | a < 0 = MinusInf

    MinusInf * a@(Val  _) = a * MinusInf
    (Val  _) * MinusInf | a > 0 = MinusInf
                        | a < 0 = PlusInf

    --------------------------------------------------
    ---- negate :: a -> a
    --------------------------------------------------
    negate PlusInf  = MinusInf
    negate MinusInf = PlusInf
    negate (Val a) = Val $ negate a

    --------------------------------------------------
    ---- abs :: a -> a
    --------------------------------------------------
    abs PlusInf = PlusInf
    abs MinusInf = PlusInf
    abs (Val a) = Val $ abs a

    --------------------------------------------------
    ---- fromInteger :: Integer -> a
    --------------------------------------------------
    fromInteger i = Val $ fromInteger i

    --------------------------------------------------
    ---- signum :: a -> a
    --------------------------------------------------
    signum MinusInf = Val (-1)
    signum PlusInf  = Val 1
    signum (Val a)  = Val $ signum a

instance Fractional Limit where
    PlusInf / (Var a)
        | a > 0 = PlusInf
        | a < 0 = MinusInf

    MinusInf / (Var a)
        | a > 0 = MinusInf
        | a < 0 = PlusInf

    _ / _ = error "Undetermined case of limit division"


type Bound = (Limit, Openness)

-- | Yeay, interval
data Interval = Interval !Bound !Bound deriving (Eq, Show)

data Domain = 
    -- | Describe an application, typically :
    -- [-inf; +inf] -> [-1;1]
    -- [0; +inf] -> [-inf; +inf]
    -- [0;1] U [2;3] -> [0;1] U [2;2.5]
      App [Interval] [Interval]
    deriving (Eq, Show)

union :: Interval -> Interval -> [Interval]
union i1@(Interval (l,kl) (h,kh)) i2@(Interval (l',kl') (h',kh'))
    | l' < l = union i2 i1
    -- [+       [- +]      -]
    -- l       l'   h       k'
    | l' < h = [Interval (l, kl) (h', kh')]
    -- [+       +]]-        -]
    -- [+       +[[-        -]
    | h == l' && (kh == Include || kl' == Include) =
        [Interval (l, kl) (h', kh')]
    -- [+       +]      [-      -]
    | otherwise = [i1, i2]

instance Ord Openness where
    (<) Include Exclude = True
    (<) Include Include = False
    (<) Exclude Include  = False
    (<) Exclude Exclude = False

instance Num Interval where
    (Interval x1 x2) + (Interval y1 y2) =
        Interval (x1 + y1) (x2 + y2)
    
    (Interval x1 x2) - (Interval y1 y2) =
        Interval (x1 - y2) (x2 - y1)

    (Interval x1 x2) * (Interval y1 y2) =
        Interval ( minimum crossProduct, maximum crossProduct )
            where crossProduct = [ x * y | x <- [x1, x2], y <- [y1, y2] ]

    abs i@(Interval x y)
        | x > 0 && y > 0 = i
        | x < 0 && y > 0 = Interval (abs x) y
        -- Here x < 0 && y < 0, x > 0 && y < 0
        -- cannot happen by definition.
        | otherwise = Interval (abs y) (abs x)
    negate (Interval x y) = Interval (negate y) $ negate x
    signum (Interval x y) = Interval (signum x) $ signum y

