all: llvm-passes rapid rts

rapid:
	idris2 --build rapidc.ipkg

rapid-lite:
	idris2 --build rapid-lite.ipkg

docs:
	$(MAKE) -C docs html

rts: external/llvm-statepoint-utils/dist/llvm-statepoint-tablegen.h
	$(MAKE) -C rts
	cp -v rts/build/runtime.bc support/rapid/runtime.bc
	cp -v rts/build/platform.a support/rapid/platform.a

external/llvm-statepoint-utils/dist/llvm-statepoint-tablegen.h:
	$(MAKE) -C external/llvm-statepoint-utils dist/llvm-statepoint-tablegen.h

llvm-passes:
	(cd llvm && test -f Makefile || cmake .)
	$(MAKE) -C llvm
	cp -v llvm/librapid.so support/rapid/librapid.so

clean: clean-tests clean-docs
	$(MAKE) -C rts clean
	$(MAKE) -C llvm clean
	rm -rf build samples/build

clean-tests:
	rm -rf tests/chez/*/build
	rm -rf tests/chez/*/output
	rm -rf tests/chez/*/compile.log

clean-docs:
	$(MAKE) -C docs clean

check: test

test: rts test-llvm
	./runtests.sh --good

test-llvm:
	$(MAKE) -C llvm test

install: rapid llvm-passes rts
	test -d `idris2 --prefix`/bin
	test -d `idris2 --libdir`/support
	cp -av support/rapid `idris2 --libdir`/support
	cp -av build/exec/rapidc{,_app} `idris2 --prefix`/bin

.PHONY: all check clean clean-docs clean-tests docs install rapid rapid-lite rts test
