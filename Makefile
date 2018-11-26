all:
	dune build stanc.exe

cross:
	dune build stanc.exe -x windows

clean:
	dune clean

re: clean all
