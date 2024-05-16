.PHONY: all build clean

all:
	cabal install --install-method=copy --installdir=./ --overwrite-policy=always
	mv xyz-interpreter-exe interpreter
#	clear
#	./interpreter --help

# Additional build option, in case we want to look for warnings and errors
# Build option does not build the program, it only checks for errors and warnings in fancy colors
build:
	cabal build

clean:
	cabal clean
	rm interpreter
