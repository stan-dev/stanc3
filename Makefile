all:
	dune build src/stanc/stanc.exe

.PHONY: doc test

test:
	dune runtest

testcoverage:
	@find . -name '*.coverage' | xargs rm -f
	dune clean
	BISECT_FILE=`pwd`/bisect dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report html --expect src/ --do-not-expect src/stancjs/
	bisect-ppx-report summary --expect src/ --do-not-expect src/stancjs/
	@rm *.coverage

format:
	dune build @fmt

profile:
	dune build src/stanc/stanc.exe --instrument-with landmarks --force

cross:
	dune build src/stanc/stanc.exe -x windows

static:
	dune build src/stanc/stanc.exe --profile static

clean:
	dune clean

doc:
	dune build @doc

re: clean all
