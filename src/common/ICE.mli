(** Internal compiler errors *)

val internal_error : string -> 'a
(** Alias for [failwith] *)

val internal_errorf :
     ('a, Format.formatter, unit, string) format4
  -> ('a, string) Format.Args.t
  -> 'b
(** Failwith but with pretty-printing *)

val ( $ ) : 'a Fmt.t -> 'a -> Format.formatter -> unit
(** [$] is [Fun.flip]; useful in internal_errorf for pairing pretty printers and
    values into something %t can format:
    [internal_errorf "Foo: %t" [pp_foo $ foo] ] is equivalent to
    [internal_error (Format.asprintf "Foo: %a" pp_foo foo)]

    A similar operator was suggested in the
    {{:https://github.com/ocaml/ocaml/pull/13372#issuecomment-2325194034}PR}
    which introduced the [Format.Args.t] type *)

val with_exn_message : (unit -> 'a) -> ('a, string) result
(** Catch all exceptions at the top-level and convert them into a
    ['a, string result] where the string contains the exception and a backtrace
    if present, followed by a link to our bugtracker. *)
