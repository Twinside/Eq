===========
Definitions
===========

In this section, the definitions are stored in files.

Definition
==========
Let's get things started slowly by `binding` a variable and
using it.

.. command-output:: cat var_bind.txt
.. command-output:: eq eval -f var_bind.txt


The ``:=`` operator let you set the value of a variable.
The right and side of the operator (3 here), is evaluated at the definition
site.

The ``;`` separate all your definitions. Only the value of the last
expression is displayed. During the evaluation of an expression if a variable
is found, we check if it's bound, if it is, we replace the variable by the
bound value and continue evaluation.

Function definition
===================
You can define function the same way than variable, but using a
slightly different syntax.

.. command-output:: cat fun_def.txt
.. command-output:: eq eval -f fun_def.txt

It's possible to have function with multiple parameters too

.. command-output:: cat fun_multi.txt
.. command-output:: eq eval -f fun_multi.txt

Mathcing
========
You may now want to define functions with many if to change behaviour
of your definitions and everything. Sad news, there is no ``if``,
no ``while`` and no ``for`` in `Eq Language`. But
the language provide an other mean to control behaviour. It's the power of...

Unification
-----------
We are now going to define a function whose behaviour change in function
of the value of it's parameters.

.. command-output:: cat fun_unif1.txt
.. command-output:: eq eval -f fun_unif1.txt

In the first call ``uniftest`` try to match the first argument (here 3)
with it's first pattern (here 3, also, nice). As these two value `Match` it then
continue with the variable ``a``. As a is a variable, it can match with
anything at least once. So the first definition of the ``uniftest``
match with the given parameters so ``2 * a`` is returned but ``a``
is substituted by ``12``.

For the second call to ``uniftest`` We try to match 1 to 3, which can't work.
So we jump to the second definition and by the same principle, ``2 * 2``
is returned.

The rule for choosing function definition is to take the first definition which
match the given argument. So definition order matters.

Example : creating a if
-----------------------
We now have all the tools necessary to create a ``if`` construct
by ourselves. By using a specific value to determine which definition of the
function to choose, we can write it easily.

.. command-output:: cat if_example.txt
.. command-output:: eq eval -f if_example.txt -->

That was easy. When you find a ``--`` token, the rest of the line is considered
as a comment. You can write ``{- multiline comment like in this sample -}``.
It's time to introduce you to some special values :
 
 * true
 * false

Which are the `boolean` values. All comparisons operators generate a boolean value.
So to define an if, it's a good thing to use them. The if got a third case, which is a bit
weird. In a normal utilisation, an ``if`` should always use boolean value as first
argument (the `conditional` argument. The reason of this definition need an example :

.. command-output:: cat pattern_failure.txt
.. command-output:: eq eval -f pattern_failure.txt; true
    :shell:

Your first matching error. Cute isn't it? So instead of getting a harsh crash,
we return the variable ``undefined``, which can help you to reuse some result later.
You can bound the ``undefined`` variable like any other variable.

Before I told you that the `Eq Language` didn't possess an if construct. I have
to admit that I lied a bit. There is an if function, defined in the standard library.
You can read the standard library (which is rather thin at the moment), but if you
modify it the modification won't be taken into account.

To learn how to loop and perform some more advanced stuff, jump to the next
section.
