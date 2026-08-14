(lang dune 3.12)

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
  (menhir
   (= 20260209))
  (fmt
   (= 0.11.0))
  (yojson
   (= 3.0.0))
  (cmdliner
   (= 2.1.1))
  (ppx_deriving
   (= 6.1.1))
  ppx_compare
  ppx_sexp_conv
  ppx_expect_nobase
  (ppx_sexp_value :with-test)
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.29.0)))
   (menhirformat
    :with-dev-setup)
  (bisect_ppx :with-dev-setup)
  (ocaml-lsp-server :with-dev-setup)
  (menhir-lsp :with-dev-setup)
  (odoc :with-doc)
  (sherlodoc :with-doc)))

(cram enable)

(generate_opam_files true)

(using menhir 2.0)

(dialect
 (name menhir)
 (implementation
  (extension mly)
  (format
   (setenv
    PATH "%{env:PATH=''}"
    (run menhirformat --indent-once %{input-file})))))

(dialect
 (name ocamllex)
 (implementation
  (extension mll)
  (format
   (setenv
    PATH "%{env:PATH=''}"
    (run menhirformat --indent-once %{input-file})))))

(using directory-targets 0.1)
