(** Used for user-facing warning messages *)

module Location_span = Middle.Location_span

type t

val stancjs_bad_include : string -> t
val jacobian_dataonly : Location_span.t -> string option -> t
val lp_in_transparam : Location_span.t -> t
val int_divide : Location_span.t -> (Format.formatter -> unit) -> t
val matrix_power : Location_span.t -> (Format.formatter -> unit) -> t

val compare_chain :
     Location_span.t
  -> (Format.formatter -> unit)
  -> (Format.formatter -> unit)
  -> t

val assign_to_self : Location_span.t -> Location_span.t -> t
val initialize_with_self : Location_span.t -> Location_span.t -> t
val unreachable_statement : Location_span.t -> Ast.complete -> t
val empty_file : Location_span.t -> t
val forward_declaration : Location_span.t -> t
val lkj_cov_deprecation : Location_span.t -> t
val function_deprecation : Location_span.t -> string -> int * int -> string -> t
val ode_deprecation : Location_span.t -> string -> int * int -> string -> t
val pedantic_warning : Location_span.t * string -> t
val pp : ?printed_filename:string -> ?code:string -> t Fmt.t
val pp_warnings : ?printed_filename:string -> ?code:string -> t list Fmt.t

val to_grace :
  ?printed_filename:string -> ?code:string -> t -> 'a Grace.Diagnostic.t
