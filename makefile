
build:
	runhaskell Setup.hs build

clean:
	runhaskell Setup.hs clean

conf:
	runhaskell Setup.hs configure

copy:
	cp dist/build/formularender/eq.exe .

run:
	dist/build/formularender/eq.exe test.txt out.txt

help:
	dist/build/formularender/eq.exe help

