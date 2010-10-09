===
EQ
===

Eq is a small formula manipulation tool.

Build instructions
==================
To build you need :

 * A recente GHC (tested with GHC 6.12)
 * Make (if you don't want to bother)

To build the project, you just have to type ::

    make conf
    make

Or alternatively, if you just want to use the haskell toolkit ::

    runhaskell Setup.hs configure
    make

You want to run make to build, as some code generation might happen.
The base library is pre-parsed during the build.

The build will produce two executables :
 * eq : the real software
 * eqtestsuite : guess what? the test suite

Debug builds
============

If you want to develop ``eq``, you might want to enable debug builds ::

    # make debug act as a configure in debug mode
    make debug
    make

Alternatively, you can request a debug build directly ::

	runhaskell Setup.hs configure --user --flags="debug profiling"
	runhaskell Setup.hs build

Documentation build
===================
There is actually two kinds of documentations :

 * Code documentation
 * Language documentation

The language documentation is in the userdoc, and you need :

 * sphinx
 * program-output (sphinx add-on)

to build it, just go the userdoc folder and type ``make``.

Code documentation generation has not been tested in a long time,
try typing make doc do generate it.

