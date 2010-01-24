
ifeq ($(shell uname),WindowsNT)

SHELL:=cmd
EXEEXT:=.exe
FIND := unixfind

else

EXEEXT:=
FIND := find

endif

EQ     := dist/build/eq/eq$(EXEEXT)
EQTEST := dist/build/eqtestsuite/eqtestsuite$(EXEEXT)

# Calculated on demand, I hope...
PARSECVER = $(shell ghc-pkg list --simple-output parsec | tr " " "\n" | grep "parsec-3\.")

build: EqManips/BaseLibrary.hs
	runhaskell Setup.hs build
	cp $(EQ) .
	cp $(EQTEST) .

clean:
	runhaskell Setup.hs clean

# only way to get this shit working on unix...
EqManips/BaseLibrary.hs: EqManips/libMaker.hs EqManips/base-library.txt
	ghc -package $(PARSECVER) --make -cpp -o libMaker EqManips/libMaker.hs
	./libMaker
	$(FIND) EqManips -name '*.o' | xargs rm
	$(FIND) EqManips -name '*.hi' | xargs rm
	$(FIND) EqManips -name '*.o-boot' | xargs rm
	$(FIND) EqManips -name '*.hi-boot' | xargs rm
	rm libMaker$(EXEEXT)

showdoc:
	echo dist\doc\html\FormulaRenderer\eq\index.html

doc:
	runhaskell Setup.hs haddock --executables

profiling:
	runhaskell Setup.hs configure --flags="profiling optimize"

debug:
	runhaskell Setup.hs configure --flags="debug profiling optimize"

release:
	runhaskell Setup.hs configure --flags="optimize"

conf:
	runhaskell Setup.hs configure --flags="optimize"

test:
	rm -f *.tix
	./eqtestsuite

hlint:
	$(FIND) . -name *.hs | grep -v dist | grep -v ErrorMessages | xargs hlint

coverage:
	hpc markup --destdir coverageReport $(EQTEST)

# This really complicated build target is there to overcome
# a GHC bug (at least in GHC 6.10.4) when linking in static.
# The compiler doesn't call the linker with a good library order.
# GHC rely on collect2 --start-gtoup & --end-group feature to
# put the most important lib first, but forget to put
# lpthread. So the idea to build, is to make as if we build
# for real (creating .o & all), let the linking crash in
# verbose mode, get back the command line, patch it
# and finaly run it again.
#
# With all this work, we add a little stripping/compression,
# just to be clean.
staticrelease: EqManips/BaseLibrary.hs
	- [ -e dist ] && rm -Rf dist
	runhaskell Setup.hs configure --flags="StaticLinking"
	- runhaskell Setup.hs build -v >> outLogFile 2>&1
	echo \#!/bin/sh > futureScript.sh
	cat outLogFile | grep collect2 | head -n 1 | sed 's/--start-group/--start-group -lpthread/' >> futureScript.sh
	chmod 700 futureScript.sh
	./futureScript.sh
	rm futureScript.sh outLogFile
	cp dist/build/eq/eq$(EXEEXT) .
	strip eq$(EXEEXT)
	upx --best eq$(EXEEXT)


