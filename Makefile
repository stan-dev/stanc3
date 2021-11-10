all:
	dune build src/stanc/stanc.exe

TEST_DUNES := $(foreach d,$(shell find test/integration -type d),$(d)/dune)
test: $(TEST_DUNES)
	dune runtest

format:
	dune build @fmt

cross:
	dune build src/stanc/stanc.exe -x windows

static:
	dune build src/stanc/stanc.exe --profile static

clean:
	dune clean

.PHONY: doc
doc:
	dune build @doc

re: clean all

test/integration/examples-good/%/dune: test/integration/examples-good/dune
	echo "(include `echo "$*/" | sed -e "s|[^/]*/|../|g"`dune)" > $@

test/integration/examples-bad/%/dune: test/integration/examples-bad/dune
	echo "(include `echo "$*/" | sed -e "s|[^/]*/|../|g"`dune)" > $@

test/integration/included/%/dune: test/integration/included/dune
	echo "(include `echo "$*/" | sed -e "s|[^/]*/|../|g"`dune)" > $@

test/integration/model-name/%/dune: test/integration/model-name/dune
	echo "(include `echo "$*/" | sed -e "s|[^/]*/|../|g"`dune)" > $@

test/integration/rstanarm/%/dune: test/integration/rstanarm/dune
	echo "(include `echo "$*/" | sed -e "s|[^/]*/|../|g"`dune)" > $@

test/integration/example-models/%/dune: test/integration/example-models/dune
	echo "(include `echo "$*/" | sed -e "s|[^/]*/|../|g"`dune)" > $@

OG_TEST_DUNES := test/integration/dune test/integration/examples-bad/dune test/integration/examples-good/dune test/integration/included/dune test/integration/model-name/dune test/integration/rstanarm/dune test/integration/example-models/dune

clean-test-dunes:
	rm -rf $(filter-out $(OG_TEST_DUNES),$(TEST_DUNES))
