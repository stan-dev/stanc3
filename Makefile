all:
	dune build stanc.exe

TEST_DUNES := $(foreach d,$(shell find test/integration -type d),$(d)/dune)
test: $(TEST_DUNES)
	dune runtest

format:
	dune build @fmt

cross:
	dune build stanc.exe -x windows

clean:
	dune clean

re: clean all

test/integration/examples-good/%/dune: test/integration/examples-good/dune
	ln -sf $(abspath $^) $@

test/integration/examples-bad/%/dune: test/integration/examples-bad/dune
	ln -sf $(abspath $^) $@
