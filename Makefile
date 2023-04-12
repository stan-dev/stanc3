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

cross:
	dune build src/stanc/stanc.exe -x windows

static:
	dune build src/stanc/stanc.exe --profile static

clean:
	dune clean

doc:
	dune build @doc

OCAMLLIBDIR=$(shell ocamlc -where)

stanc-bytecode:
	$(eval SEXLIB_DUNE=$(OCAMLLIBDIR)/../sexplib/dune-package)
	sed -i'.bak' -e 's/requires bigarray/requires/g' $(SEXLIB_DUNE)
	dune build src/stanc/stanc.bc.c
	$(RM) $(SEXLIB_DUNE)
	mv $(SEXLIB_DUNE).bak $(SEXLIB_DUNE)

stanc.c: stanc-bytecode
	mkdir stanc_c_files
	mkdir stanc_c_files/include
	cp _build/default/src/stanc/stanc.bc.c stanc_c_files/.
	cp -r /Users/andrew/.opam/stanc/lib/ocaml/caml stanc_c_files/
	mkdir stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/floats.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/backtrace_byt.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/alloc.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/array.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/bigarray.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/backtrace.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/str.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/ints.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/io.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/compare.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/dynlink.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/stacks.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/weak.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/eventlog.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/finalise.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/gc_ctrl.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/meta.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/hash.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/intern.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/signals.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/obj.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/md5.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/memprof.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/memory.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/extern.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/parsing.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/callback.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/afl.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/sys.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/unix.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/startup_aux.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/startup_byt.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/codefrag.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/domain.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/misc.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/fix_code.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/skiplist.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/signals_byt.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/minor_gc.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/major_gc.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/globroots.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/compact.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/lexing.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/interp.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/custom.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/printexc.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/runtime/debugger.c stanc_c_files/runtime

	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/otherlibs/str/strstubs.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/otherlibs/systhreads/st_stubs.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/otherlibs/systhreads/freelist.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/otherlibs/systhreads/roots_byt.c stanc_c_files/runtime
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/otherlibs/systhreads/fail_byt.c stanc_c_files/runtime

	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/otherlibs/systhreads/st_win32.h stanc_c_files/include
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ocaml-base-compiler.4.12.0/otherlibs/systhreads/st_posix.h stanc_c_files/include


	mkdir stanc_c_files/base
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ppx_inline_test.v0.14.1/runner/lib/am_testing.c stanc_c_files/base
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/base.v0.14.3/hash_types/src/internalhash_stubs.c stanc_c_files/base
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/base.v0.14.3/hash_types/src/internalhash.h stanc_c_files/include
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/base.v0.14.3/src/hash_stubs.c stanc_c_files/base
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/base.v0.14.3/src/exn_stubs.c stanc_c_files/base
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/base.v0.14.3/src/int_math_stubs.c stanc_c_files/base

	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/base_bigstring.v0.14.0/src/base_bigstring_stubs.c stanc_c_files/base
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/base_bigstring.v0.14.0/src/base_bigstring.h stanc_c_files/include

	mkdir stanc_c_files/bin_prot
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/bin_prot.v0.14.1/src/blit_stubs.c stanc_c_files/bin_prot

	mkdir stanc_c_files/time_now
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/time_now.v0.14.0/src/time_now_stubs.c stanc_c_files/time_now

	mkdir stanc_c_files/ppx_expect
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/ppx_expect.v0.14.2/collector/expect_test_collector_stubs.c stanc_c_files/ppx_expect

	mkdir stanc_c_files/core_kernel
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/core_kernel/src/array_stubs.c stanc_c_files/core_kernel
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/core_kernel/src/bigstring_stubs.c stanc_c_files/core_kernel
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/core_kernel/src/md5_stubs.c stanc_c_files/core_kernel
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/core_kernel/src/core_bigstring.h stanc_c_files/include

	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/jane-street-headers.v0.14.0/include/core_params.h stanc_c_files/include
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/jane-street-headers.v0.14.0/include/ocaml_utils.h stanc_c_files/include
	cp /Users/andrew/.opam/stanc/lib/ocaml/../../.opam-switch/sources/jane-street-headers.v0.14.0/include/jane_common.h stanc_c_files/include

re: clean all
