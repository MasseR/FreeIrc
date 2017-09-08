.PHONY: build test

all: build

build:
	stack setup
	stack build

test: build
	stack test
