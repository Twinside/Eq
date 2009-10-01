
SHELL = cmd
MAKESHELL = cmd

build: dll

clean:
	unixfind . | grep "\.o$$" | xargs rm -f
	unixfind . | grep "\.hi$$" | xargs rm -f
	rm formulaDll_stub*
	rm formulaDll.dll.a
	rm formulaDll.dll

dll:
	ghc -c --make -cpp formulaDll.hs
	ghc -c dllMain.cpp
	unixfind . | grep "\.o$$" | xargs ghc -shared \
										-o formulaDll.dll \
										-package parsec \
										-package array \
										-package mtl \
										-package containers \
										-package filepath \
										formulaDll.def
