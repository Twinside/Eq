.. _lambda:

======
Lambda
======

Creation
========

.. function:: Lambda(Argument, Body)
    Create an anonymouse function taking one argument. When
    the lambda is found in the formula, variables not present
    in it's argument list are bound to variable in the environment
    before any call of the lambda function.

The Lambda function in EQ is just a limited version of the full function
declaration, but can instantiated in any formula, depending of other
parameters.

.. command-output:: eq eval 'x := 3; (Lambda( x, x * x ))( 4 )'; true
    :shell:

