open Middle

type originblock =
  | MathLibrary
  | Functions
  | Data
  | TData
  | Param
  | TParam
  | Model
  | GQuant
[@@deriving sexp]

type varinfo = {origin: originblock; global: bool; readonly: bool}
[@@deriving sexp]

type info =
  { type_: UnsizedType.t
  ; kind:
      [ `Variable of varinfo
      | `UserDeclared of Location_span.t
      | `StanMath
      | `UserDefined ] }
[@@deriving sexp]

type t

val create : unit -> t
val find : t -> string -> info list

val add :
     t
  -> string
  -> Middle.UnsizedType.t
  -> [ `UserDeclared of Location_span.t
     | `StanMath
     | `UserDefined
     | `Variable of varinfo ]
  -> t

val add_all_raw : t -> string -> info list -> t
val mem : t -> string -> bool
val iter : t -> (info list -> unit) -> unit

val returntype :
     t
  -> string
  -> (UnsizedType.autodifftype * UnsizedType.t) list
  -> ( UnsizedType.returntype * (bool Middle.Fun_kind.suffix -> Ast.fun_kind)
     , SignatureMismatch.signature_error list * bool )
     result
