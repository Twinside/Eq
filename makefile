
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

run:
	dist/build/eq/eq.exe preprocess -f tests/preprocess/test.cc -o rez.cc

help:
	dist/build/eq/eq.exe help

