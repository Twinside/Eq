
build:
	runhaskell Setup.hs build
	cp dist/build/eq/eq.exe .

clean:
	runhaskell Setup.hs clean

doc:
	runhaskell Setup.hs haddock --executables

conf:
	runhaskell Setup.hs configure

copy:
	cp dist/build/eq/eq.exe .

buildtest:
	ghc -O2 --make -o iotest.exe -cpp .\EqManips\Tests\iotestSuite.hs

test:
	./iotest

run:
	dist/build/eq/eq.exe preprocess -f tests/preprocess/test.cc -o rez.cc
	dist/build/eq/eq.exe preprocess -f rez.cc -o rez2.cc
	dist/build/eq/eq.exe preprocess -f tests/preprocess/test.c -o rez.c
	dist/build/eq/eq.exe preprocess -f rez.c -o rez2.c

help:
	dist/build/eq/eq.exe help

