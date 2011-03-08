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

run:
	#./eq eval "(1 + 3 * x + 2 * x^2 - 7 * x ^3) / (1 + x - 2 * x ^2)"
	./eq plot "log(x)" > rez 2>&1
	./eq plot "tan(x)" >> rez 2>&1
	./eq plot "sin(x)" >> rez 2>&1
	./eq plot "sin(x) * 3" >> rez 2>&1
	./eq plot "sin(x) * 5" >> rez 2>&1
	./eq plot "sin(x) / 3" >> rez 2>&1
	./eq plot "exp(x)" >> rez 2>&1
	./eq plot --logheight "exp(x)" >> rez 2>&1

dll:
	ghc $(DEBUG) -c --make -cpp formulaDll.hs
	ghc $(DEBUG) -c dllMain.c
	unixfind . | grep "\.o$$" | sed -e 's:\\:/:g' | \
				xargs ghc $(DEBUG) -shared -optl-mwindows \
										-o formulaDll.dll \
										-package parsec \
										-package array \
										-package mtl \
										-package containers \
										-package filepath \
										-package utf8-string\
										-package HaXml \
										-lOle32 \
										formulaDll.def

sharedlib:
	ghc -O2 --make -cpp  -no-hs-main -optl '-shared' -optc '-DMODULE=FormulaDll' -o eqlinlib.so formulaDll.hs module_init.c

