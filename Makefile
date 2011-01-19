TARGETS=cabal-dev/bin/clue-demo cabal-dev/bin/clue-tests

all: ${TARGETS}

test: cabal-dev/bin/clue-tests
	cabal-dev/bin/clue-tests

cabal-dev/bin/%: dist/build
	cabal-dev install

dist/build: dist/setup-config $(shell find src -name \*.hs) $(shell find tests -name \*.hs)
	cabal-dev build

dist/setup-config: Clue.cabal
	cabal-dev configure

clean:
	find src tests -name \*.hi -delete
	rm -rf dist bin
