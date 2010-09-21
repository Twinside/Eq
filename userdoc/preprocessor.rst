============
Preprocessor
============


Why a preprocessor?
===================
Yes, I can hear you, but yes, you need a preprocessor for 
some equations. For a better documentation, you can inline
a nice ASCII art equation, and for some formula manipulation,
you can directly process it within your source code.

Example
=======
.. command-output:: cat ../tests/preprocess/formattest.c

The important point here is the special comment. Let's see
the result before explaining it.

.. command-output:: eq preprocess -o temp.c -f ../tests/preprocess/formattest.c
.. command-output:: cat temp.c

Source code markup
==================
The little embedded markup format is really simple. To use it, you
must write it in <strong>mono line</strong> comment. Let's digg a bit
in the definition which trigger the processing :

========== ============================================================================================
``Eq:``    Tell Eq that he can process something
``format`` The eq command to call, you are limited here, you don't have all the command at disposition.
``<@<``    Begin marker of output
``>@>``    End marker of output
========== ============================================================================================

The ``<@<`` and ``>@>`` are ugly, but
they permit the preprocessor to re-analyze the file and update
the previous render, if someone got a better idea, please send
me a mail about it.

Supported languages
===================

.. command-output:: eq show --languages

As you can see, many languages are already supported for formatting.
Some language name are common due to the similarity of their mono line
comment format. If you want your favorite language to be included here,
please drop me a mail.

For the moment Eq use the file extension to guess the language. It
might change in the future.
