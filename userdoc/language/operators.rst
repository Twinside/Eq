.. _operators:

=========
Operators
=========

All operators are followed by their priority. Lower
is applied before bigger priorities.

Unary operators (name - description)
====================================
 * ``-`` Negation operator, put it before expression (-x)
 * ``!`` Factorial operator, put it after expression (x!)

Definition operators
====================
 * 8: ``:=`` Attribution operator
 * 8: ``:>`` Lazy attribution operator

You can see :ref:`vardef` and :ref:`fundef` for their use

Binary operators (Priority - name - description)
================================================
 * 7: ``::`` List appending operator
 * 6: ``&``  Logical and operator
 * 6: ``|``  Logical or operator
 * 4: ``+``  Addition operator
 * 4: ``-``  Subtraction operator
 * 3: ``*``  Multiplication operator
 * 3: ``/``  Division/fraction operator
 * 2: ``^``  Power operator

You can see :ref:`lists`for the use of ``::``
``&`` and ``|`` are only defined for :ref:`boolean`.

Comparison operators
====================
Scalar comparison operators, only give a result if both
operand is a scalar number.

 * 5: ``<`` Lower than operator
 * 5: ``>`` Greater than operator
 * 5: ``>=`` Greater or equal operator
 * 5: ``<=`` Lower or equal operator

Equality operators
==================
Eq possess two operators :

 * 5: ``=`` for scalar equality.
 * 5: ``!=`` for scalar inequality.

These two operators perform uniquely on values, not on
formula structure, if you want depp comparison, you should
look at :ref:`matching` or at :ref:`eqlib`

Related
=======
| See :ref:`matrix` for the restriction on the available operators on matrices.
| See :ref:`matching` or at :ref:`eqlib` for deep equality.

