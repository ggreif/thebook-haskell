CABALSANDBOX := ".cabal-sandbox"

.PHONY: bench clean format haddock hpc init install repl run test

all: install format hpc bench haddock run

bench: install
	cabal build
	cabal bench --benchmark-options="-o report.html"

clean:
	cabal clean
	if test -d .cabal-sandbox; then cabal sandbox delete; fi
	if test -d .hpc; then rm -r .hpc; fi

format:
	git ls-files '*.hs' | xargs -n 1 scan --inplace-modify
	git ls-files '*.hs' | xargs stylish-haskell --inplace

haddock:
	cabal configure
	cabal build
	cabal haddock --hyperlink-source
	# dist/doc/html/threase/index.html

hpc: test
	hpc report dist/hpc/tix/hspec/hspec.tix
	hpc markup --destdir=tmp dist/hpc/tix/hspec/hspec.tix
init:
	cabal update
	cabal sandbox init

install: init
	cabal install --enable-benchmarks --enable-tests --flags=documentation --only-dependencies

repl:
	cabal configure
	cabal build
	cabal repl

run:
	cabal configure
	cabal build
	cabal run

test: install
	cabal configure --enable-tests
	cabal build
	cabal test
