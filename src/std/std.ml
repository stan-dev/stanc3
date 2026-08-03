(** Extensions to the standard library. Most files in the project should begin
    [open Std]. *)

(** New modules *)

module Nonempty_list = Nonempty_list

(** OCaml Stdlib with labeled functions *)

include StdLabels
include MoreLabels

(** A few extensions to builtin modules *)

module Option = struct
  include Option

  let first_some a b = Option.blend Fun.const a b
end

module String = struct
  include String

  let chop_suffix ~suffix s =
    if ends_with ~suffix s then Some (drop_last (length suffix) s) else None
end
