.. _eqlib:

============
Eq's library
============

.. function:: max( a, b )

    Return the biggest value between a and b

    .. command-output:: eq eval "max( 2, 10.0 )"

.. function:: min( a, b )

    Return the smallest value between the two elements

    .. command-output:: eq eval "min( 2, 10.0 )"

.. function:: eq( a, b )

    Perform a structural (deep) equality comparison

    .. command-output:: eq eval "eq( x + 3, x + 4 )"
    .. command-output:: eq eval "eq( x + 3, x + 3 )"

.. function:: if( boolean, then, else )

    Return either then or else in function of the
    first argument. If it's true, then it return then,
    if it's false it return else. If it's neither true
    or false, then it return undefined

    .. command-output:: eq eval "if( 3 > 4, lower, greater )"
    .. command-output:: eq eval "if( 3 < 4, lower, greater )"
    .. command-output:: eq eval "if( 3, lower, greater)"

.. function:: derivaten( function, var, order )

    Derivate `order` times the function with the given var

    .. command-output:: eq eval "derivaten( x ^ 4 + x ^ 3 / 2 + 15 * x, x, 1 )"
    .. command-output:: eq eval "derivaten( x ^ 4 + x ^ 3 / 2 + 15 * x, x, 2 )"
    .. command-output:: eq eval "derivaten( x ^ 4 + x ^ 3 / 2 + 15 * x, x, 3 )"

.. function:: modulo( n, p )

    give the value of n modulo p

    .. command-output:: eq eval "modulo( 14, 3 )"

.. function:: taylor( formula, derivation var, onVar, order )

    taylor( f, var, a, n ) :> Sort( Cleanup( taylorin( Lambda( var, f ), var, a, n )))

.. function:: cons(a, b)

    Is just the functional version of the `::` operator


List functions
==============
.. function:: concat( alist, anotherList )

    Return the concatenation of two lists.

    .. command-output:: eq eval "concat([a, b, c], [1, 2, 3] )"

.. function:: reverse( list )

    Return the list reversed

    .. command-output:: eq eval "reverse([a, b, c])"

.. function:: length( list )

    Return the length (number of elements) of a list

    .. command-output:: eq eval "length([a, b, c, 10])"

List-generation functions
=========================
.. function:: listFromTo( beginning, end )

    Generate a list containing all the integer from beginning
    to end (included).

    .. command-output:: eq eval "listFromTo( 12, 30 )"

.. function:: listFromToBy( beginning, increment, maximum )

    Generate a list of numbers containing all the integers
    from beginning to end, each value separated by increment.

    .. command-output:: eq eval "listFromToBy( 12, 2, 30 )"

Higher-order functions
======================
.. function:: filter( function, list )

    Remove all object which the function doesn't evaluate
    to true.

    .. command-output:: eq eval "enough(x) :> x > 50; filter( enough, [100, 2, 4, 51, 50, 60 ] )"

.. function:: map( function, list )

    Apply a `function` to all elements of the list

    .. command-output:: eq eval "mul2(x) :> x * 2; map( mul2, [1, 2, 3, 12] )"

.. function:: foldl( function, accumulator, list )

.. function:: foldr( function, accumulator, list )
