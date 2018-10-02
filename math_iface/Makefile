print_sigs:
	cd ../stan2; make print_sigs; cp print_sigs ../math_iface

sigs.txt: print_sigs
	./print_sigs > sigs.txt

math.cpp: sigs.txt gen_math_cpp.py
	python gen_math_cpp.py < sigs.txt > math.cpp

math.o: math.cpp
	$(COMPILE.cc) math.cpp -o math.o
