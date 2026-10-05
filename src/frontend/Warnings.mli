(** Used for user-facing warning messages *)

module Location_span = Middle.Location_span

type t =
  | JacobianFunCallDataOnly of Location_span.t * string option
  | LpInTransformedParam of Location_span.t
  | IntDivide of Location_span.t * string
  | MatrixPower of Location_span.t * string
  | ChainedCompare of Location_span.t * string * string
  | AssignToSelf of Location_span.t * Location_span.t
  | InitializeWithSelf of Location_span.t * Location_span.t
  | Unreachable of Location_span.t * Ast.complete
  | EmptyFile
  | Deprecation of Location_span.t * string
  | Pedantic of Location_span.t * string

val pp : ?printed_filename:string -> ?code:string -> t Fmt.t
val pp_warnings : ?printed_filename:string -> ?code:string -> t list Fmt.t

val to_grace :
  ?printed_filename:string -> ?code:string -> t -> 'a Grace.Diagnostic.t
