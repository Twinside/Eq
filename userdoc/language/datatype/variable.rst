.. _vardef:

=========
Variables
=========

In this section, the definitions are stored in files.

Variables
=========
In Eq all variables are defined at the top level and followed by a ``;``.
You cannot redefine a variable :

.. command-output:: eq eval 'a := 3; a :=3'; true
    :shell:

Defining a variable
===================
Two define a variable, you can use two operators, the first one and simpler
the attribution operator (same used in previous example)

.. command-output:: cat docexample/var_bind.txt
.. command-output:: eq eval -f docexample/var_bind.txt

The ``:=`` operator let you set the value of a variable.
The right and side of the operator (3 here), is evaluated at the definition
site.

The ``;`` separate all your definitions. Only the value of the last
expression is displayed. During the evaluation of an expression if a variable
is found, we check if it's bound, if it is, we replace the variable by the
bound value and continue evaluation.

Variable evaluation
===================
When a variable is found in a formula, a previous definition
is searched for it. If one exists, the variable in the formula
is substituted by it's definition. Once the substitution is done,
the definition of the variable is evaluated.

If no definition exist for the variable, the variable stay
as-is, with no modification.

Lazy attribution
================
The difference between the ``:=`` (attribution) and ``:>`` (lazy attribution)
operator reside in the evaluation of the right hand side. The attribution
operator evaluate it, whereas the lazy one doesn't. It can be really
important for the definition of some recursive functions.

Related
=======
| See :ref:`fundef` for function definition.

