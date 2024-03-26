(** Internal compiler errors *)

open Core

(** An alias of [Core.raise_s]. This used to do more
    processing, for now it is preserved just as a nicer
    marker in the code *)
let internal_compiler_error = raise_s

(** Catch all exceptions at the top-level and convert them into a ['a, string result]
    where the string contains the exception and a backtrace if present, followed
    by a link to our bugtracker. *)
let with_exn_message f =
  try Ok (f ())
  with e ->
    let bt =
      if Printexc.backtrace_status () then Printexc.get_backtrace ()
      else "Backtrace missing." in
    Error
      (Fmt.str
         "Fatal error:@ @[%a@]@\n\
          %s@\n\
          @\n\
          This should never happen. Please file a bug at \
          https://github.com/stan-dev/stanc3/issues/new@ and include this \
          message and the model that caused this issue.@\n"
         Exn.pp e bt)
