(** Internal compiler errors *)

open Core

(** Equivalent to [raise_s] but prepends a stanc specific
  message asking users to report a bug *)
let internal_compiler_error message =
  let augmented =
    Sexplib0.Sexp.List
      [ [%message
          "Fatal error: this should never happen. Please file a bug on \
           https://github.com/stan-dev/stanc3/issues/new and include the model \
           that caused this issue."]; message ] in
  raise_s augmented
