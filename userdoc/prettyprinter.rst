=================
 Pretty printing
=================

Pretty printing in a terminal
=============================
Eq can be used just to create an <i>ASCII Art</i> representation
of an equation. To do this, type the following command in a command
line:

.. command-output:: eq format "sqrt( 3 * 3 / 4! )"

Pretty printing to a file
=========================
For your convenience, you also could store the result of the command
line into a file. With great originality, doing this require using the
-o switch.

.. command-output:: eq format -o foo.txt "sqrt( 3 * 3 / 4! )" 

.. command-output:: cat foo.txt
    
Pretty printing from a file
===========================
If you want to pretty print a formula stored in a file,
if it's the result of another program for example. You can
use the -f switch.

.. command-output:: echo '3 * 5 / 2' > foo.txt
    :shell:

.. command-output:: eq format -f foo.txt

And you can combine every switch in every way, store a file in a
file whil reading from another.

