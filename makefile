
build:
	runhaskell Setup.hs build
	cp dist/build/eq/eq.exe .
	cp dist/build/iotest/iotest.exe .

clean:
	runhaskell Setup.hs clean

showdoc:
	echo dist\doc\html\FormulaRenderer\eq\index.html

doc:
	runhaskell Setup.hs haddock --executables

conf:
	runhaskell Setup.hs configure

test:
	./iotest

debug:
	./eq.exe eval -o out.txt -f taylor.txt

run:
	dist/build/eq/eq.exe preprocess -f tests/preprocess/test.cc -o rez.cc
	dist/build/eq/eq.exe preprocess -f rez.cc -o rez2.cc
	dist/build/eq/eq.exe preprocess -f tests/preprocess/test.c -o rez.c
	dist/build/eq/eq.exe preprocess -f rez.c -o rez2.c

help:
	dist/build/eq/eq.exe help

