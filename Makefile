all:
	dune build src/stanc/stanc.exe

.PHONY: doc test testcoverage format cross static clean re

test:
	dune runtest

PARALLEL ?= auto
BISECT_FLAGS = --expect src/ --do-not-expect src/stancjs/ --do-not-expect src/stan_math_signatures/Generate.ml --do-not-expect src/frontend/parser_strip_redundant_state.ml --do-not-expect src/frontend/parser_messages_add_type.ml


testcoverage: clean
	BISECT_FILE=`pwd`/bisect dune runtest --instrument-with bisect_ppx --force --root=. -j$(PARALLEL)
	bisect-ppx-report html $(BISECT_FLAGS)
	bisect-ppx-report summary $(BISECT_FLAGS)
	bisect-ppx-report coveralls coverage.json $(BISECT_FLAGS)
	@rm *.coverage

format:
	dune build @fmt

cross:
	dune build src/stanc/stanc.exe -x windows

static:
	dune build src/stanc/stanc.exe --profile static

clean:
	dune clean

doc:
	dune build @doc

re: clean all
