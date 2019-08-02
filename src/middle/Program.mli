open Core_kernel
open Common

type 'a fun_def = 'a Mir_pattern.fun_def =
  { fdrt: UnsizedType.t option
  ; fdname: string
  ; fdargs: (UnsizedType.autodifftype * string * UnsizedType.t) list
  ; fdbody: 'a
  ; fdloc: Location_span.t sexp_opaque [@compare.ignore] }
[@@deriving compare, hash, map, sexp, map]

type io_block = Mir_pattern.io_block =
  | Parameters
  | TransformedParameters
  | GeneratedQuantities
[@@deriving sexp, hash]

val pp_io_block : Format.formatter -> io_block -> unit

type 'a outvar = 'a Mir_pattern.outvar =
  { out_unconstrained_st: 'a SizedType.t
  ; out_constrained_st: 'a SizedType.t
  ; out_block: io_block }
[@@deriving sexp, map, hash]

type ('a, 'b) t = ('a, 'b) Mir_pattern.prog =
  { functions_block: 'b fun_def list
  ; input_vars: (string * 'a SizedType.t) list
  ; prepare_data: 'b list (* data & transformed data decls and statements *)
  ; log_prob: 'b list (*assumes data & params are in scope and ready*)
  ; generate_quantities: 'b list (* assumes data & params ready & in scope*)
  ; transform_inits: 'b list
  ; output_vars: (string * 'a outvar) list
  ; prog_name: string
  ; prog_path: string }
[@@deriving sexp, map]

module Make_traversable (A : Applicative.S) :
    Bitraversable.S with module A := A and type ('a, 'b) t := ('a, 'b) t

module Make_traversable2 (A : Applicative.S2) :
    Bitraversable.S2 with module A := A and type ('a, 'b) t := ('a, 'b) t

include Pretty.S2 with type ('a, 'b) t := ('a, 'b) t

module Typed : sig
  type nonrec t = (Expr.Typed.t, Stmt.Located.t) t

  include Pretty.S with type t := t
end

module Labelled : sig
  type nonrec t = (Expr.Labelled.t, Stmt.Labelled.t) t

  include Pretty.S with type t := t

  val label : ?init:int -> Typed.t -> t

  val associate :
    ?init:Stmt.Labelled.associations -> t -> Stmt.Labelled.associations
end


val functions_block: ('a,'b) t -> 'b fun_def list
val input_vars: ('a,'b) t -> (string * 'a SizedType.t) list
val prepare_data: ('a,'b) t ->'b list (* data & transformed data decls and statements *)
val log_prob: ('a,'b) t -> 'b list (*assumes data & params are in scope and ready*)
val generate_quantities: ('a,'b) t -> 'b list (* assumes data & params ready & in scope*)
val transform_inits: ('a,'b) t -> 'b list
val output_vars: ('a,'b) t -> (string * 'a outvar) list
val prog_name:('a,'b) t -> string
val prog_path:('a,'b) t ->  string 