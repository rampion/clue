TARGETS=bin/clue-demo bin/clue-tests

all: ${TARGETS}

test: bin/clue-tests
	bin/clue-tests

bin/%: dist/build
	mkdir -p bin
	cp dist/build/$(notdir $@)/$(notdir $@) bin

dist/build: dist/setup-config $(shell find src -name \*.hs) $(shell find tests -name \*.hs)
	runhaskell Setup.hs build

dist/setup-config: Clue.cabal
	runhaskell Setup.hs configure --user

clean:
	find src tests -name \*.hi -delete
	rm -rf dist bin
