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
	echo "(include `echo "$*/" | sed -e "s|[^/]*/|../|g"`dune)" > $@

test/integration/examples-bad/%/dune: test/integration/examples-bad/dune
	echo "(include `echo "$*/" | sed -e "s|[^/]*/|../|g"`dune)" > $@

OG_TEST_DUNES := test/integration/dune test/integration/examples-bad/dune test/integration/examples-good/dune test/integration/included/dune test/integration/model-name/dune

clean-test-dunes:
	rm -rf $(filter-out $(OG_TEST_DUNES),$(TEST_DUNES))
