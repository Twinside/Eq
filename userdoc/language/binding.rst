.. _binding:

=======
Binding
=======

Function and variable definition got two forms of bindings, early and lazy.

Early binding
=============

Let's try to write a modulo function.

.. command-output:: cat docexample/modulo101.txt

Here we have written a loop within the ``modintern`` function, the condition of
the loop is symbolized by the first argument, unifying to a boolean. The rest can be
seen as a local variable, as for the num. We remove the modulo while the rest is still
greater than it. So, now let's try it.

.. command-output:: eq eval -f docexample/modulo101.txt; true
    :shell:

As you can see, we got some errors, it's due to the early binding used.

Early binding problem
---------------------

The ``:=`` operator let you set the value of a variable.
The right and side of the operator (3 here), is evaluated at the definition site. 

    ``modintern( false, rest, num ) := modintern( rest - num < num, rest - num, num );``

The first arrise when we use the second definition of the function. We already have defined
a part of the function. As the ``:=`` operator evaluate it's right part, it tries
to match ``( rest - num < num, rest - num, num )`` As those variables are not yet
defined, they cannot be reduced to a boolean. And as you can imagine
``true =~= (rest - num < num, rest - num, num)`` is false. (I use ``=~=``
as a shorthand for matching, don't try to use it, it doesn't exist, yet). In the same
way the difinition of the modulo function can only fail.

Lazy binding
============

Now, to define our function and get it to work, we need to use the
lazy binding operator :

.. command-output:: cat docexample/modulo606.txt

.. command-output:: eq eval -f docexample/modulo606.txt

The ``:>`` operator is the `Lazy attribute operator`. It has exactly
the same function as ``:=``, but it doesn't evaluate it's right side. It
kept it's right side frozen, the right side is `unfrozen` when the function
is called. All the functions in the standard library are defined with the `Lazy`
operator to avoid problem at definition site.

