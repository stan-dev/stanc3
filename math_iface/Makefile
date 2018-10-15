math.a: gen/math.o
	ar rcs $@ $^

gen/print_sigs:
	mkdir -p gen
	cd ../stan2; make print_sigs; mv print_sigs ../math_iface/gen/print_sigs

.DELETE_ON_ERROR: gen/sigs.txt gen/filtered_sigs.txt gen/math.cpp

gen/sigs.txt: gen/print_sigs
	$< > $@

gen/filtered_sigs.txt: gen/sigs.txt filter_sigs.py
	python filter_sigs.py < $< > $@

gen/math.cpp: gen/filtered_sigs.txt gen_math_cpp.py
	python gen_math_cpp.py < $< > $@

MATH=../stan2/lib/stan_math/
include $(MATH)make/libraries

CXXFLAGS+=-std=c++1y -Wno-unknown-warning-option -Wno-tautological-compare -Wno-sign-compare -O3  -I$(MATH) -I$(EIGEN) -I$(BOOST) -I$(SUNDIALS)/include -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION -DGTEST_USE_OWN_TR1_TUPLE -DGTEST_USE_OWN_TR1_TUPLE

gen/math.o: gen/math.cpp
	$(COMPILE.cc) -o $@ $<

gen/math_c.o: iface.cpp
	$(COMPILE.cc) -static -o $@ $<

gen/test: test.c gen/math_c.o
	cc -lstdc++ -o $@ $^

gen/libmath_c.a: gen/math_c.o
	ar rcs $@ $^

clean:
	rm -f gen/math.o gen/math.cpp gen/filtered_sigs.txt gen/sigs.txt
