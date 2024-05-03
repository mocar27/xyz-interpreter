.PHONY: all build clean

all:
	cabal install --install-method=copy --installdir=./ --overwrite-policy=always
	mv xyz-interpreter-exe interpreter

build:
	cabal build

clean:
	cabal clean
	rm interpreter
