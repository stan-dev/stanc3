all:
	dune build stanc.exe

test:
	dune runtest

format:
	dune build @fmt

cross:
	dune build stanc.exe -x windows

clean:
	dune clean

re: clean all
