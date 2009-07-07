
build:
	runhaskell Setup.hs build

clean:
	runhaskell Setup.hs clean

conf:
	runhaskell Setup.hs configure

run:
	dist/build/formularender/formularender.exe test.txt out.txt

