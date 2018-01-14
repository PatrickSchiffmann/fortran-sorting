.PHONY: run all

RESULT_NAME = $(shell hostname)

CXX=icpc
FC=ifort
LD=ifort

CXXFLAGS?=-O3 -std=c++11 -pthread -lbenchmark
FCFLAGS?= -O3
LDFLAGS?= -O3 -pthread -lbenchmark -nofor_main -lstdc++

SRC_DIR := ../src/
OBJ_DIR := ./obj

CPP_SRC_FILES := $(wildcard $(SRC_DIR)/*.cpp)
CPP_OBJ_FILES := $(patsubst $(SRC_DIR)/%.cpp,$(OBJ_DIR)/%.cpp.o,$(CPP_SRC_FILES))

FC_SRC_FILES := $(wildcard $(SRC_DIR)/*.f90)
FC_OBJ_FILES := $(patsubst $(SRC_DIR)/%.f90,$(OBJ_DIR)/%.f90.o,$(FC_SRC_FILES))

all: perftest

perftest: $(CPP_OBJ_FILES) $(FC_OBJ_FILES) $(OBJ_DIR)/perftest.o $(OBJ_DIR)/fortran_wrapper.o
	$(LD) -o $@ $^  $(LDFLAGS)

$(OBJ_DIR)/perftest.o: ./perftest.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

$(OBJ_DIR)/fortran_wrapper.o: fortran_wrapper.f90
	$(FC) $(FCFLAGS) -I$(SRC_DIR) -c -o $@ $<

$(OBJ_DIR)/%.cpp.o: $(SRC_DIR)/%.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

$(OBJ_DIR)/%.f90.o: $(SRC_DIR)/%.f90
	$(FC) $(FCFLAGS) -I$(SRC_DIR) -c -o $@ $<

run: perftest
	./perftest

results: perftest
	./perftest --benchmark_format=csv > results/$(RESULT_NAME).csv

