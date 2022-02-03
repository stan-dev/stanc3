all:
	dune build src/stanc/stanc.exe

.PHONY: doc test

test:
	dune runtest

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
