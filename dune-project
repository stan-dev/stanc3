(lang dune 2.8)

(using menhir 2.0)

(name stanc)

(cram enable)

(generate_opam_files true)

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
   (= 0.9.0))
  (yojson
   (= 2.1.0))
  (ocamlformat
   (and
    :with-test
    (= 0.26.1)))
  (bisect_ppx :with-test)
  (merlin :with-test)
  (utop :with-test)
  (ocp-indent :with-test)
  (patdiff :with-test)))
