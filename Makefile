.PHONY: all run

main: src/main.f90
	ifort -fpp -prof-gen=srcpos -O0 -g -warn all src/main.f90 -o main

run: main
	./main
