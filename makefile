
build:
	runhaskell Setup.hs build

clean:
	runhaskell Setup.hs clean

conf:
	runhaskell Setup.hs configure

copy:
	cp dist/build/formularender/formularender.exe .

run:
	dist/build/formularender/formularender.exe test.txt out.txt

help:
	dist/build/formularender/formularender.exe help

