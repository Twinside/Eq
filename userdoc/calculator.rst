============
 Calculator
============


Basic calculation
=================
Eq can perform simple calculations, by calling it on the command line
and giving it an expression as parameter.

.. program-output:: eq exacteval "3 + 3"
    :prompt:

Here you can see the term exact eval. There is two important command
for program/expression evaluation.

 * exacteval : evaluate without compromising precision,
   no operation involving float are performed.
 * eval : try to reduce an expression as much as possible,
   without regard for precision. If you want to obtain a
   numerical answer, that's the command to use.

    Let's illustrate the difference

.. program-output:: eq exacteval "3 / 9"
    :prompt:

.. program-output:: eq eval "3 / 9"
    :prompt:

And for some trigonometry :

.. program-output:: eq exacteval "sin(2)"
    :prompt:

.. program-output:: eq eval "sin(2)"
    :prompt:

Operators
=========
You can use the usual operators ``(+, -, *, /)`` for addition, subtraction,
multiplication and division respectively.

You can also use some comparison operators :

 * ``=``  equality
 * ``/=`` inequality.
 * ``<``  lower than
 * ``>``  greater than
 * ``>=`` greater or equal
 * ``<=`` lower or equal


.. program-output:: eq eval "3 * 6 < 15" 
    :prompt:

.. program-output:: eq eval "3 * 6 > 15" 
    :prompt:

Know which operator are available
=================================
Some time you won't have documentation directly available. But
you still can ask Eq which operators does he knows.

.. program-output:: eq show --operators
    :prompt:

