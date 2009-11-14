SHELL = cmd
MAKESHELL = cmd
DEBUG := -debug
build: dll

clean:
	unixfind . | grep "\.o$$" | xargs rm -f
	unixfind . | grep "\.hi$$" | xargs rm -f
	rm formulaDll_stub*
	rm formulaDll.dll.a
	rm formulaDll.dll

# 
dll:
	ghc $(DEBUG) -c --make -cpp formulaDll.hs
	ghc $(DEBUG) -c dllMain.cpp
	unixfind . | grep "\.o$$" | xargs ghc $(DEBUG) -shared -optl-mwindows \
										-o formulaDll.dll \
										-package parsec \
										-package array \
										-package mtl \
										-package containers \
										-package filepath \
										-package utf8-string\
										-package HaXml \
										-lOle32 \
										formulaDll.def

