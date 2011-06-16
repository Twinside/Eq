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

build:
	runhaskell Setup.hs build
	cp $(EQ) .
	cp $(EQTEST) .

clean:
	runhaskell Setup.hs clean

showdoc:
	echo dist\doc\html\FormulaRenderer\eq\index.html

doc:
	runhaskell Setup.hs haddock --executables

profiling:
	runhaskell Setup.hs configure --user --flags="profiling optimize"

debug:
	runhaskell Setup.hs configure --user --flags="debug optimize"

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
	echo "" > rez
	./eq plot "log(x)" > rez 2>&1
	./eq plot "tan(x)" >> rez 2>&1
	./eq plot "sin(x)" >> rez 2>&1
	./eq plot "sin(x) * 3" >> rez 2>&1
	./eq plot "sin(x) * 5" >> rez 2>&1
	./eq plot "sin(x) / 3" >> rez 2>&1
	./eq plot "exp(x)" >> rez 2>&1
	./eq plot --zeroaxis "sin(x) * 5" >> rez 2>&1
	./eq plot --zeroaxis --xaxis "sin(x) * 5" >> rez 2>&1
	./eq plot --zeroaxis --yaxis --xaxis "sin(x) * 5" >> rez 2>&1
	./eq plot -y 0.5 --ye 100 --logheight --xaxis --zeroaxis "exp(x)" >> rez 2>&1
	./eq plot --logheight --xaxis "exp(x)" >> rez 2>&1
	./eq plot --yaxis -y 0.5 --ye 100 --yaxis --logheight --xaxis --zeroaxis "exp(x / 3)" --zeroaxis >> rez 2>&1
	./eq plot --zeroaxis --logheight --yaxis --xaxis "exp(x)" >> rez 2>&1
	./eq plot --logwidth -x 0.01 --xaxis "log(x)" >> rez 2>&1
	./eq plot -t "loglog" --logwidth -x 0.01 --yaxis --xaxis "log(x)" >> rez 2>&1

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

