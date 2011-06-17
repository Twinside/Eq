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

.. function:: taylor( formula, derivation var, atPos, order )

    Return the taylor expansion of an expression. For exemple
    to get the taylor expansion of the formula ``sin(x) + x``
    for the variable x and at 0, you should try :

    .. command-output:: eq exacteval "taylor( sin(x) + x, x, 0, 10 )"

.. function:: cons(a, b)

    Is just the functional version of the `::` operator


List functions
==============
.. function:: length( list )

    Return the length (number of elements) of a list

    .. command-output:: eq eval "length([a, b, c, 10])"

.. function:: concat( alist, anotherList )

    Return the concatenation of two lists.

    .. command-output:: eq eval "concat([a, b, c], [1, 2, 3] )"

.. function:: reverse( list )

    Return the list reversed

    .. command-output:: eq eval "reverse([a, b, c])"

.. function:: zip( list_a, list_b )

    Combine two list into a list of list, each sublist
    having an element of a as first element, and an element
    of b as second element.

    .. command-output:: eq eval "zip([1, 2, 3], [a, b, c])"

.. function:: replicate( count, element )

    Create a list with element repeated count times

    .. command-output:: eq eval "replicate(8, a_variable)"

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

    Function used to perform a left fold on a list.
    The function should take two parameters : one
    for an accumulator, and one for an element of the list.

    The accumulator is carried over all the elements of the list,
    and the iterations start at the beginning of the list. One
    example of the use of fold is to write the equivalent of the
    reverse function using a left fold :

    .. command-output:: eq eval "foldl( cons, [], [1, 2, 3, 4] )"

    .. _a link: http://en.wikipedia.org/wiki/Fold_%28higher-order_function%29
    
.. function:: foldr( function, accumulator, list )

    Same as the left fold, but here is the right fold, iterations
    start from the end of the list to the beginning.

    .. _a link: http://en.wikipedia.org/wiki/Fold_%28higher-order_function%29

