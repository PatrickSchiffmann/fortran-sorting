.PHONY: all run

main: src/main.f90
	ifort -O0 -g -warn all src/main.f90 -o main

run: main
	./main
