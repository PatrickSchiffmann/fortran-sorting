.PHONY: all run clean

all: main codecov 

main: src/*.f90
	ifort -fpp -O3 -warn all src/*.f90 -o main

codecov: src/*.f90
	ifort -fpp -prof-gen=srcpos -O0 src/*.f90 -o codecov
	./codecov
	profmerge
	codecov

run: main
	./main

clean:
	rm -f *.mod src/*.mod
	rm -rf *.dyn pgopti* CodeCoverage CODE_COVERAGE.HTML

