TARGETS=src/Demo

GHC_FLAGS=-Wall -fno-warn-unused-do-bind
all: ${TARGETS}

${TARGETS}:
	cd $(dir $@) && ghc $(GHC_FLAGS) --make $(notdir $@) 

clean:
	rm -f ${TARGETS} 
	find . -name \*.hi -or -name \*.o -delete

