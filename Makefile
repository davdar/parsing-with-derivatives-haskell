
PROG_NAME = der-parser

TEST_FILE = src/Test.hs

GHC_FLAGS = -Wall -isrc:../SimpleTesting/src -odir odir -hidir hidir

main: build-test

init:
	mkdir -p bin

build-test: init
	ghc --make ${GHC_FLAGS} -o bin/${PROG_NAME}-test ${TEST_FILE}

test: build-test
	bin/${PROG_NAME}-test

test-interact: build-test
	ghci -fth ${GHC_FLAGS} ${TEST_FILE}

clean:
	if test -d bin; then rm -r bin; fi
	if test -d odir; then rm -r odir; fi
	if test -d hidir; then rm -r hidir; fi

.SILENT:
