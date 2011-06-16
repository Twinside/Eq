{-# LANGUAGE QuasiQuotes #-}
module Language.Eq.BaseLibrary( defaultSymbolTable ) where

import Language.Eq.Quasiquote
import Language.Eq.Types
import Language.Eq.Renderer.Ascii()
import qualified Data.Map as M

defaultSymbolTable :: M.Map String (Formula ListForm)
defaultSymbolTable = M.fromList [eqDefs|

-- derivaten( function, var, order )
derivaten( f, var, 0 ) :> f;
derivaten( f, var, 1 ) :> derivate( {f}, {var} );
derivaten( f, var, n ) :> derivate( {derivaten( f, var, n-1 )}
                                  , {var} );

-- if( condition (boolean), then, else )
if(      true, a, b ) :> a;
if(     false, a, b ) :> b;
if( otherwise, a, b ) :> undefined;

-- map( function, list )
map( f,        [] ) :> [];
map( f,   x :: xs ) :> {f}( x ) :: map( {f}, xs );
map( f, otherwise ) :> undefined;

-- foldl( function :: acc -> elem -> acc, accumulator, list )
foldl( f, acc,      [] ) :> acc;
foldl( f, acc, x :: xs ) :> foldl( f, f( acc, x ), xs );
foldl( a,   b,       c ) :> undefined;

-- foldr( function :: acc -> elem -> acc , accumulator, list )
foldr( f, acc, []      ) :> acc;
foldr( f, acc, x :: xs ) :> f( foldr( f, acc, xs ), x );
foldr( a,   b,       c ) :> undefined;

-- zip :: ( [a], [b] ) -> [[a, b]]
zip( [], a ) :> [];
zip(  b, []) :> [];
zip( x :: xs, y :: ys ) :> [x, y] :: zip( xs, ys );

-- replicate :: Int -> a -> [a]
replicate(0, a) :> [];
replicate(n, a) :> a :: replicate(n - 1, a);

-- just to provide a function englobing list appending
-- operator
cons( a, b ) :> b :: a;

-- list reversal.
reverse( lst ) :> foldl( cons, [], lst );

-- concatenate two lists.
concat(      [],  y ) :> y;
concat(       x, [] ) :> x;
concat( x :: xs,  y ) :> x :: concat( xs, y );
concat(       a,  b ) :> undefined;

-- Filtering function, remove un-needed stuff
filter( pred,      [] ) :> [];
filter( pred, x :: xs ) :> concat( if( pred( x ), [x], [])
                                 , filter( pred, xs ) );
filter(    a,       b ) :> undefined;

listFromTo( a, a ) :> [a];
listFromTo( a, b ) :> a :: listFromTo( a + 1, b );

listFromToBy( a, by, a ) :> [a];
listFromToBy( a, by, maxi ) :> a :: listFromToBy( a + by, by, maxi );

lengthIntern( acc,      [] ) :> acc;
lengthIntern( acc, x :: xs ) :> lengthIntern( acc + 1, xs );
lengthIntern(   a,       b ) :> undefined;

-- length
length( lst ) :> lengthIntern( 0, lst );

-- well, a max function
max( a, b ) :> if( a > b, a, b );

-- well, a min function
min( a, b ) :> if( a < b, a, b );

-- provide equality when everything else is undefined :-P
eq( a, a ) :> true;
eq( a, b ) :> false;

-- modintern( n<p, rest, module )
modintern(  true, rest, num ) :> rest;
modintern( false, rest, num ) :> modintern( rest - num < num, rest - num, num );

-- give the value of n modulo p
modulo( n, p ) :> modintern( n < p, n, p );

-- taylor( function (as a lambda!!), derivation var, onVar, order )
taylorin( f, var, a, 0 ) :> f(a);
taylorin( f, var, a, n ) :> taylorin( f, var, a, n - 1 ) 
                          + (derivaten(f, var, {n}))( a ) / n! * (x - a) ^ n;

-- taylor( formula, derivation var, onVar, order )
taylor( f, var, a, n ) :>
    Sort( Cleanup( taylorin( Lambda( {var}, {f} )
                           , var, a, n )))

|]


