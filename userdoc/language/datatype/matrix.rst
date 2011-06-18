.. _matrix:

======
Matrix
======

Matrix creation
===============

Matrix from arguments
---------------------

.. function:: matrix( width, height, ... )

    Create a matrix given a size and a list of arguments

    :param width: number of columns in the matrix
    :param height: number of lines in the matrix
    
.. command-output:: eq eval "matrix( 2, 2, a, b, c, d )"

The :func:`matrix` matrix function take at least 3 arguments :
 #. The width of the matrix
 #. The height of the matrix
 #. At least one element.

The number of argument must be equal to ``width * height``. You can
define column and row matrices

.. command-output:: eq eval "matrix( 1, 4, a, b, c, d )"
.. command-output:: eq eval "matrix( 4, 1, a, b, c, d )"

Matrix from list
----------------
.. function:: matrix( lists )
    Create a matrix given a list of lists

You also can build matrices directly from lists (which can be dynamically generated)

.. command-output:: eq eval "matrix( [1, 4, a, b, c, d ] )"
.. command-output:: eq eval "matrix( [[1, 0], [0, 1]])"

Matrix from function
--------------------
To generate from a size and a function with two parameters, you can
use the :py:func:`generateMatrix` function, from Eq's base library.

Supported operators
===================

Matrices support only a subset of all the operators, between them only
addition and multiplication are supported (if they size match for the
operation that is).

.. command-output:: eq eval "matrix( 2, 2, a, b, c, d ) + matrix( 2, 2, e, f, g, h )"
.. command-output:: eq eval "matrix( 2, 2, a, b, c, d ) * matrix( 2, 2, e, f, g, h )"

Only two operators are allowed between matrices and scalars, multiplication
and division.

.. command-output:: eq eval "matrix( 1, 4, a, b, c, d ) * (x+y)"
.. command-output:: eq eval "matrix( 4, 1, a, b, c, d ) / x^2"

Matrix information
==================

.. function:: matrixWidth( matrix )

    Return the width (number of columns) of the matrix.

.. command-output:: eq eval "matrix([[1, 2, 3]])"
.. command-output:: eq eval "matrixWidth(matrix([[1, 2, 3]]))"

.. function:: matrixHeight( matrix )

    Return the height (number of lines) of the matrix.

.. command-output:: eq eval "matrixHeight(matrix([[1, 2, 3]]))"

Indexation
----------
TODO!

Related
=======
See :ref:`lists` for their creation/generation.

