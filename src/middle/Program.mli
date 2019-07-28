open Core_kernel
open Common

type 'a fun_def = 'a Mir_pattern.fun_def =
  { fdrt: UnsizedType.t option
  ; fdname: string
  ; fdargs: (UnsizedType.autodifftype * string * UnsizedType.t) list
  ; fdbody: 'a
  ; fdloc: Location_span.t sexp_opaque [@compare.ignore] }
[@@deriving compare, hash, map, sexp,  map]

type io_block = Mir_pattern.io_block = Parameters | TransformedParameters | GeneratedQuantities
[@@deriving sexp, hash]

val pp_io_block : Format.formatter -> io_block -> unit
type 'a outvar = 'a Mir_pattern.outvar = 
    { out_unconstrained_st: 'a SizedType.t
    ; out_constrained_st: 'a SizedType.t
    ; out_block: io_block }
    [@@deriving sexp, map, hash]


module Fixed : sig 
    module Pattern : sig
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

        include Pretty.S2 with type ('a,'b) t := ('a,'b) t
    end

    type ('a,'b) t = ('a Expr.Fixed.t, ('a,'b) Stmt.Fixed.t) Pattern.t
    include Pretty.S2 with type ('a,'b) t := ('a,'b) t
end 

module Typed : sig
    type t = (Expr.Typed.Meta.t,Stmt.Located.Meta.t) Fixed.t
    include Pretty.S with type t := t
end