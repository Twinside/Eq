===
sum
===

The `Eq Language` provide a sum (and a product) function, which can be abused
in some ways. It's a good to know how to pervert it and how to get
it straight otherwise.

Sum for formatting
==================
When you use a sum (or a product) in formatting, there isn't any rules.
You must at least give it an expression as parameter. If you add more
parameters, they will be used as bounds for the sum.

.. command-output:: eq format 'sum(k)'

The last parameter is allways the summed expression.

.. command-output:: eq format 'sum(a, k)'

Here whe use the product function. Both function work the same way

.. command-output:: eq format 'sum(a, b, k)'

If you use to many parameters, your sum (or product is invalid), and
will not be formatted.

.. command-output:: eq format 'sum(a, b, c, k)'

Evaluation of sums
==================
For a successful evaluation of sum, certain rules must be followed :

 #. All bounds must be defined.
 #. A variable must be used in the lowerbound and given an equality.
 #. Lower bound (given as equality) and upper bound must be integral.

The first rule is really simple to test :

.. command-output:: eq eval 'sum(3)'

As you see, no evaluation has happened. To understand the second and third rules,
we ha ve to see a working example.

.. command-output:: eq eval '1 + sum( n = 1, 4, x ^ n / n! )'

By using ``n = 1`` we tell the evaluator to use <code>n</code> as the iteration
variable. We also tell it to start at 1. The upper bound tell to stop after 4. As you
can see the bounds are integers, there is no point in the number. As an exemple, by
slightly modifying the previous example, we obtain the following result :

.. command-output:: eq eval '1 + sum( n = 1, 4.0, x ^ n / n! )'

The floating point version of 4 stop the evaluator to perform some more processing.
As some may have noticed, the previous samples are the taylor expension of the
exponential function in 0.

.. function:: sum( initialisation, upperbound, formula )

    :param initialisation: Must be in form ``var = expr`` the expr part is evaluated
    :param upperbound: Must be an expression, is evaluated at runtime.
    :param formula: The formula to be summed

Product for evaluation
======================
The same rules apply for the product, this section is just
here to provide a working example

.. command-output:: eq eval 'product( n = 1, 10, n )'
.. command-output:: eq eval 'product( n = 1, 10, n ) = 10!'

.. function:: product( initialisation, upperbound, formula )

    :param initialisation: Must be in form ``var = expr`` the expr part is evaluated
    :param upperbound: Must be an expression, is evaluated at runtime.
    :param formula: The formula to be multiplied

