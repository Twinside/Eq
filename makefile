
ifneq ($(shell uname),Linux)

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

