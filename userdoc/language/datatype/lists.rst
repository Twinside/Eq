.. _lists:

=====
Lists
=====

Creating a list
===============
The eq languate permit the creation of lists, wich is a collection
of expression separated by commas and between ``[]``

.. command-output:: eq eval '[1, 2, 3]'

for a list of simple numbers

.. command-output:: eq eval '[3 * 2, x, y / 2]'

for a list of more complex content.

You also can build a list from the empty list ``[]``
and using the `cons` (or append) operator noted ``::``

.. command-output:: eq eval '1 :: 3 :: []'

Don't try to format an expression directly by writing
some ``::`` into it, because it won't be translated to
a real list during the formating.

.. command-output:: eq format '1 :: 3 :: []'

General front-appending
=======================
You can append any element at the front of an existing list
using the cons operator (``::``)

.. command-output:: eq eval 'x :: [1, 2, 3]'

Indexing a list
===============
List can be indexed to get back a specific element of it.

.. command-output:: eq format '[3 * 2, x, y / 2] _ 2'
.. command-output:: eq eval '[3 * 2, x, y / 2] _ 2'

The indices go from 1 to length of the list (included).

Deconstruction of a list
========================
You can pattern match a list, either directly, or using
the ``::`` operator. First let's look to the exact matching

.. command-output:: cat docexample/list_matching_full.txt
.. command-output:: eq eval -f docexample/list_matching_full.txt

It's really simple, we try to match, the first match is the most precise,
the one returning list_of_three accept all list of length three and the
last match accept anything. Now we can try to deconstruct a list element
by element.

.. command-output:: cat docexample/ignorefirst.txt
.. command-output:: eq eval -f docexample/ignorefirst.txt

You can think at the cons matching of a list as if you've built your
list only with `::`. We can also imagine a function which return half
of the list, returning only the odd or even elements.

.. command-output:: cat docexample/list_odds.txt
.. command-output:: eq eval -f docexample/list_odds.txt

There is more information on list processing in the description of
the library's functions.

Related
=======
| See :ref:`matrix` for it's use of lists and it's creation.
| See :ref:`matching` for more information on pattern matching.

