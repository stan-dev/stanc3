all:
	dune build src/stanc/stanc.exe

.PHONY: doc test update-rules testcoverage format cross static clean re

test: update-rules
	dune runtest

# Regenerate the per-model rules in test/integration/**/expected_*/dune.inc
# for added or removed models, and delete expected files of removed models.
update-rules:
	@dune build @update-rules --auto-promote 2>/dev/null || dune build @update-rules
	test/integration/remove_orphan_expected.sh

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
