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
   (= 5.5.0))
  (base
   (= v0.17.3))
  (stdio
   (= v0.17.0))
  (sexplib0
   (= v0.17.0))
  (ppx_base
   (= v0.17.0))
  (ppx_pipebang
   (= v0.17.0))
  (ppx_sexp_message
   (= v0.17.0))
  (ppx_sexp_value
   (= v0.17.0))
  (ppx_expect
   (= v0.17.3))
  (ppx_inline_test
   (= v0.17.1))
  (menhir
   (= 20260209))
  (ppx_deriving
   (= 6.1.1))
  (fmt
   (= 0.11.0))
  (yojson
   (= 3.0.0))
  (cmdliner
   (= 2.1.1))
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.28.1)))
  (bisect_ppx :with-dev-setup)
  (ocaml-lsp-server :with-dev-setup)
  (odoc :with-doc)
  (sherlodoc :with-doc)))

(cram enable)

(generate_opam_files true)

(using menhir 2.0)

(using directory-targets 0.1)
