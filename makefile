
ifeq ($(shell uname),WindowsNT)

SHELL:=cmd
EXEEXT:=.exe
FIND := unixfind

else

EXEEXT:=
FIND := find

endif


build: EqManips/BaseLibrary.hs
	runhaskell Setup.hs build
	cp dist/build/eq/eq$(EXEEXT) .
	cp dist/build/deq/deq$(EXEEXT) .
	cp dist/build/iotest/iotest$(EXEEXT) .

clean:
	runhaskell Setup.hs clean

# only way to get this shit working on unix...
EqManips/BaseLibrary.hs: EqManips/libMaker.hs EqManips/base-library.txt
	ghc -package 'parsec-3.0.0' --make -cpp -o libMaker EqManips/libMaker.hs
	./libMaker
	$(FIND) EqManips -name '*.o' | xargs rm
	$(FIND) EqManips -name '*.hi' | xargs rm
	$(FIND) EqManips -name '*.o-boot' | xargs rm
	$(FIND) EqManips -name '*.hi-boot' | xargs rm
	rm libMaker

showdoc:
	echo dist\doc\html\FormulaRenderer\eq\index.html

doc:
	runhaskell Setup.hs haddock --executables

conf:
	runhaskell Setup.hs configure

test:
	rm -f *.tix
	./iotest

debug:
	runhaskell EqManips/Tests/polyTest.hs

run:
	./deq eval -o out.txt -f tests/programm/valid/metaTest.txt

help:
	dist/build/eq/eq$(EXEEXT) help

hlint:
	$(FIND) . -name *.hs | grep -v dist | grep -v ErrorMessages | xargs hlint

coverage:
	hpc markup --destdir coverageReport iotest

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
staticrelease:
	- [ -e dist ] && rm -Rf dist
	mv formulaRenderer.cabal temp.cabal.tmp
	cp formulaStatic.cabal.wait formulaRenderer.cabal
	runhaskell Setup.hs configure
	- runhaskell Setup.hs build -v >> outLogFile 2>&1
	echo \#!/bin/sh > futureScript.sh
	cat outLogFile | grep collect2 | head -n 1 | sed 's/--start-group/--start-group -lpthread/' >> futureScript.sh
	chmod 700 futureScript.sh
	./futureScript.sh
	rm futureScript.sh outLogFile
	rm formulaRenderer.cabal
	mv temp.cabal.tmp formulaRenderer.cabal
	cp dist/build/eq/eq$(EXEEXT) .
	strip eq$(EXEEXT)
	upx --best eq$(EXEEXT)


