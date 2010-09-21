======
Matrix
======

Matrix creation
===============
.. command-output:: eq eval "matrix( 2, 2, a, b, c, d )"

The matrix function take at least 3 arguments :
 #. The width of the matrix
 #. The height of the matrix
 #. At least one element.

The number of argument must be equal to <code>width * height</code>. You can
define column and row matrixes

.. command-output:: eq eval "matrix( 1, 4, a, b, c, d )"
.. command-output:: eq eval "matrix( 4, 1, a, b, c, d )"

Supported operators
===================
.. command-output:: eq eval "matrix( 2, 2, a, b, c, d ) + matrix( 2, 2, e, f, g, h )"
.. command-output:: eq eval "matrix( 2, 2, a, b, c, d ) * matrix( 2, 2, e, f, g, h )"

And for the scalars...

.. command-output:: eq eval "matrix( 1, 4, a, b, c, d ) * (x+y)"
.. command-output:: eq eval "matrix( 4, 1, a, b, c, d ) / x^2"

