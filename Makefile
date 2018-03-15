.PHONY: build test nixrepl

all: test

nixrepl:
	hpack -f
	cabal2nix ./. > default.nix
	touch shell.nix
	cabal repl

build:
	./cibuild.sh

test: build
