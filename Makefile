.PHONY: all build clean

all:
	cabal install --install-method=copy --installdir=./ --overwrite-policy=always
	mv xyz-interpreter-exe interpreter

# Additional build option, in case we want to look for warnings and errors
build:
	cabal build

clean:
	cabal clean
	rm interpreter
