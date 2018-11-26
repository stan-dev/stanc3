all:
	jbuilder build stanc.exe

cross:
	jbuilder build stanc.exe -x windows

clean:
	jbuilder clean

re: clean all
