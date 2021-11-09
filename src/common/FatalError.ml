(** Internal compiler errors *)

open Core_kernel

(** Equivalent to [Common.FatalError.fatal_error_msg] but prepends a stanc specific
  message asking users to report a bug *)
let fatal_error_msg message =
  let augmented =
    Sexplib0.Sexp.List
      [ [%message
          "Fatal error: this should never happen. Please file a bug on \
           https://github.com/stan-dev/stanc3/issues/new."]; message ]
  in
  raise_s augmented

(** A version of [fatal_error_msg] with an empty message.
  The resulting error only includes the issues link *)
let fatal_error () = fatal_error_msg [%message]
