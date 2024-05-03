.PHONY: all clean

all:
	cabal install --install-method=copy --installdir=./ --overwrite-policy=always
	cabal build -j1
	mv dist-newstyle/build/aarch64-osx/ghc-9.8.2/xyz-interpreter-0.1.0.0/x/xyz-interpreter-exe/build/xyz-interpreter-exe/xyz-interpreter-exe interpreter

clean:
	cabal clean
	rm interpreter
	rm xyz-interpreter-exe
