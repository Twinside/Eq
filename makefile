SHELL=cmd

build: EqManips/BaseLibrary.hs
	runhaskell Setup.hs build
	cp dist/build/eq/eq.exe .
	cp dist/build/deq/deq.exe .
	cp dist/build/iotest/iotest.exe .

clean:
	runhaskell Setup.hs clean

EqManips/BaseLibrary.hs: EqManips/libMaker.hs EqManips/base-library.txt
	runhaskell -cpp EqManips\libMaker.hs

showdoc:
	echo dist\doc\html\FormulaRenderer\eq\index.html

doc:
	runhaskell Setup.hs haddock --executables

conf: EqManips/BaseLibrary.hs
	runhaskell Setup.hs configure

test:
	./iotest

debug:
	runhaskell EqManips/Tests/polyTest.hs

run:
	./eq exacteval -o out.txt "taylor( exp(x), x, 0, 5)"

help:
	dist/build/eq/eq.exe help

hlint:
	unixfind . -name *.hs | grep -v dist | grep -v ErrorMessages | xargs hlint

