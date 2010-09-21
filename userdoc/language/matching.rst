========
Matching
========

We've barely scratched the surface of matching as implemented
in Eq. We now need to digg into the subject by following a tiny
example. I've not yet implemented a modulo operator in the language,
so it's a nice occasion to implement one ourselves.

Modulo
======
Let's write our modulo in the most straightforward way.
.. command-output:: cat modulo101.txt

Here we have written a loop within the ``modintern`` function, the condition of
the loop is symbolized by the first argument, unifying to a boolean. The rest can be
seen as a local variable, as for the num. We remove the modulo while the rest is still
greater than it. So, now let's try it.

.. command-output:: eq eval -f modulo101.txt; true
    :shell:

We got two errors we some cryptic messages (yes it's in my <strong>`TODO`</strong>
list to ameliorate them). But what the OMGWTFBBQ!!!!111!!!ELEVEN!!! has happenned?
the problem is `:=`

The `:=` problem
================
The ``:=`` operator let you set the value of a variable.
The right and side of the operator (3 here), is evaluated at the definition site. 

    ``modintern( false, rest, num ) := modintern( rest - num < num, rest - num, num );``

The first arrise when we use the second definition of the function. We already have defined
a part of the function. As the ``:=`` operator evaluate it's right part, it tries
to match ``( rest - num &lt; num, rest - num, num )`` As those variables are not yet
defined, they cannot be reduced to a boolean. And as you can imagine
``true =~= (rest - num &lt; num, rest - num, num)`` is false. (I use ``=~=``
as a shorthand for matching, don't try to use it, it doesn't exist, yet). In the same
way the difinition of the modulo function can only fail.

A working modulo
================
So, new program :

.. command-output:: cat modulo606.txt

Notice the new ``:>`` operator.

.. command-output:: eq eval -f modulo606.txt


The ``:>`` operator is the `Lazy attribute operator`. It has exactly
the same function as ``:=``, but it doesn't evaluate it's right side. It
kept it's right side frozen, the right side is `unfrozen` when the function
is called. All the functions in the standard library are defined with the `Lazy`
operator to avoid problem at definition site.

Deep comparison
===============

.. command-output:: eq eval "x - 3 = x - 3"

The ``=`` is thought to compare equality on value,
but some times you want to check deep equality, like in the
previous sample. The two sub-tree are identical, you could
try to write some weird function deconstructing the two
subexpression to provide equality. Or you can try a very
simple trick.

.. command-output:: eq eval "equal( a, a ) :> true; equal( a, b ) :> false; equal( x - 3, x - 3)"

``equal( a, a )`` does all the job, the first argument match himself
with the variable, and then a is substituted by the first argument in the argument
list and in the function body. Then the second argument has a complex tree to
match to. If it does, the first equation is chosen, giving a deep equality check.
Otherwise, the all accepting clause ``equal( a, b )`` is selected
and reject the equality.

