(lang dune 3.5)

(using menhir 2.0)

(using directory-targets 0.1)

(name stanc)

(cram enable)

(generate_opam_files true)

(package
 (name stanc)
 (synopsis "The Stan compiler and utilities")
 (depends
  (ocaml
   (= 5.4.0))
  (core
   (= v0.17.1))
  (menhir
   (= 20240715))
  (ppx_deriving
   (= 6.0.3))
  (fmt
   (= 0.11.0))
  (yojson
   (= 2.2.2))
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
