.. _boolean:

=======
Boolean
=======

Thruthness constants
====================
In `Èq` There is two magic constant for truthness :

.. _true _false:
 * ``true``
 * ``false``

They cannot be used directly in arithmetic operations, but
can be combined using the logic operators (:ref:`operators`)

Generating truthness using comparison
=====================================
Truthness is used as parameter to some functions (notably the :ref:py:if one),
and can be created using one of the comparison operator.

.. command-output:: eq eval '[3 > 5, 3 < 5]'

Combining truthness
===================
``true`` and ``false`` react to the well known operators
``&`` (and) and ``|`` (or)

.. command-output:: eq eval '[true & false, false & true, false & false, true & true ]'
.. command-output:: eq eval '[true | false, false | true, false | false, true | true ]'

