
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
PARSECVER = $(shell ghc-pkg list --simple-output parsec | tr " " "\n" | grep "parsec-3\." | tail -1)

build: EqManips/BaseLibrary.hs
	runhaskell Setup.hs build
	cp $(EQ) .
	cp $(EQTEST) .

clean:
	runhaskell Setup.hs clean

# only way to get this shit working on unix...
EqManips/BaseLibrary.hs: EqManips/libMaker.hs EqManips/base-library.eq
	ghc -package $(PARSECVER) --make -cpp -o libMaker EqManips/libMaker.hs
	./libMaker
	$(FIND) EqManips -name "*.o" | xargs rm
	$(FIND) EqManips -name "*.hi" | xargs rm
	$(FIND) EqManips -name "*.o-boot" | xargs rm
	$(FIND) EqManips -name "*.hi-boot" | xargs rm
	rm libMaker$(EXEEXT)

showdoc:
	echo dist\doc\html\FormulaRenderer\eq\index.html

doc:
	runhaskell Setup.hs haddock --executables

profiling:
	runhaskell Setup.hs configure --user --flags="profiling optimize"

debug:
	runhaskell Setup.hs configure --user --flags="debug profiling optimize"

release:
	runhaskell Setup.hs configure --user --flags="optimize"

conf:
	runhaskell Setup.hs configure --user --flags="optimize"

test:
	rm -f *.tix
	./eqtestsuite

hlint:
	$(FIND) . -name *.hs | grep -v dist | grep -v ErrorMessages | xargs hlint

coverage:
	hpc markup --destdir coverageReport $(EQTEST)

# With all this work, we add a little stripping/compression,
# just to be clean.
staticrelease: EqManips/BaseLibrary.hs
	- [ -e dist ] && rm -Rf dist
	runhaskell Setup.hs configure --user --flags="optimize StaticLinking"
	- runhaskell Setup.hs build
	cp dist/build/eq/eq$(EXEEXT) .
	strip eq$(EXEEXT)
	upx --best eq$(EXEEXT)


