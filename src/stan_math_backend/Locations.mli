open Middle
open Common

module Numbered : sig
  module Meta : sig
    type t = int [@@deriving compare, sexp, hash]

    include Meta.S with type t := t
  end

  include
    Specialized.S
    with module Meta := Meta
     and type t = (Expr.Typed.Meta.t, Meta.t) Stmt.Fixed.t
end

type typed_prog_num = (Expr.Typed.t, Numbered.t) Program.t [@@deriving sexp]
type state_t

val prepare_prog : Program.Typed.t -> typed_prog_num * state_t
val pp_globals : Format.formatter -> state_t -> unit
val pp_smeta : Format.formatter -> int -> unit
val no_span_num : int
