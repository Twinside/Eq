
build:
	runhaskell Setup.hs build
	cp dist/build/eq/eq.exe .

clean:
	runhaskell Setup.hs clean

conf:
	runhaskell Setup.hs configure

copy:
	cp dist/build/eq/eq.exe .

run:
	dist/build/eq/eq.exe test.txt out.txt

help:
	dist/build/eq/eq.exe help

