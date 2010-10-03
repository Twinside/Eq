.. _lambda:

======
Lambda
======

Creation
========

.. function:: Lambda(Argument, Body)

    :param Argument: unevaluated parameter name
    :param Body: unevaluated body of the lambda

    Create an anonymouse function taking one argument. When
    the lambda is found in the formula, variables not present
    in it's argument list are bound to variable in the environment
    before any call of the lambda function.

    The Lambda function in EQ is just a limited version of the full function
    declaration, but can instantiated in any formula, depending of other
    parameters.

    .. command-output:: eq eval 'x := 3; (Lambda( x, x * x ))( 4 )'

    The argument and the body are not evaluated when executed.

Substitution in lambda
======================
You can control if a substitution will happen by using one of the
evaluation controlling function :

.. command-output:: eq eval 'x := 3; (Lambda( x, Force(x) * x ))( 4 )'

You can see :ref:`evalControl` for further information

