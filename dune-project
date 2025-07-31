(lang dune 3.5)

(name stanc)

(source
 (github stan-dev/stanc3))

(license BSD-3-Clause)

(documentation https://mc-stan.org/stanc3/stanc/)

(package
 (name stanc)
 (synopsis "The Stan compiler and utilities")
 (depends
  (ocaml
   (= 4.14.1))
  (core
   (= v0.16.1))
  (menhir
   (= 20230608))
  (ppx_deriving
   (= 5.2.1))
  (fmt
   (= 0.11.0))
  (yojson
   (= 2.1.0))
  (cmdliner
   (= 2.1.0))
  (ocamlformat
   (and
    :with-test
    (= 0.28.1)))
  (bisect_ppx :with-test)
  (merlin :with-test)
  (utop :with-test)
  (patdiff :with-test)))

(cram enable)

(generate_opam_files true)

(using menhir 2.0)

(using directory-targets 0.1)
