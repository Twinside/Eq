.. _evalControl:

==================
Evaluation control
==================

The evaluation control function are in fact what is called
'meta functions'.

Evaluation control
==================
.. function:: Hold( subexpr )

	When this function is found in an evaluated zone, it preserve
	it's content from any substitution/reduction. Use it if you
	want to avoid transformation of some formula's part.

.. function:: Force( subexpr )

	This function is the inverse Hold, when found in area without
	evaluation, it force the evaluation of it's sub expression.
	
	.. command-output:: eq eval "d(x) :> derivate( x, y ); d( y + 5 + 13 * y )"

    The previous command doesn't work, because the derivate function doesn't
    evaluate both of it's argument, if you want a substitution you have to force
    it :

	.. command-output:: eq eval "d(x) :> derivate( Force(x), y ); d( y + 5 + 13 * y )"

    And you can use a more concise form of the form function :

	.. command-output:: eq eval "d(x) :> derivate( {x}, y ); d( y + 5 + 13 * y )"

Other meta functions
====================

.. function:: Expand( formula )

	Experimental function, use at your own risk.

.. function:: Cleanup( formula )

	Remove trivial-simplification (multiplication by 1/0), just
	to cleanup more things. Normally performed internally.

.. function:: Sort( formula )

	Sort terms of a formula or a list in ascending order.

	.. command-output:: eq eval "Sort([15, 2, 3, x, a])"

