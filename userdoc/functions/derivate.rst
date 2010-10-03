========
Derivate
========

Example
=======

.. function:: derivate( formula, var )

    :param formula: The unevaluated formula
    :param var: The unevaluated derivation variable


    Examples :

    .. command-output:: eq format "derivate( x^4 + sin(x^2) - ln(x) * exp(x) + 7, x )"
    .. command-output:: eq eval "derivate( x^4 + sin(x^2) - ln(x) * exp(x) + 7, x )"

    You can force evaluation in formula and var.

Non-allowed derivations
=======================
Now it's time to try a derivative with problems, by adding a
non-variable inside it's definition.

.. command-output:: eq eval "derivate( x^4 + sin(x^2) - ln(x) * exp(x) + 7, x * x )"; true
    :shell:

What you can see after the error text and the error expression, 
is the Eq's favourite error value. <code>#</code> cannot be typed
into expression, but it's often generated to indicate error. If you
find some, well, good luck :)

