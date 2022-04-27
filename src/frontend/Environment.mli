(** Type environments used during typechecking. Maps from strings to function or variable information *)

open Middle

(** Origin blocks, to keep track of where variables are declared *)
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

(** Information available for each variable *)
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

val make_from_library :
     ( string
     , ( UnsizedType.returntype
       * (UnsizedType.autodifftype * UnsizedType.t) list
       * Common.Helpers.mem_pattern )
       list )
     Core_kernel.Hashtbl.t
  -> t
(** Make a type environment from a hashtable of functions like those from
    [Std_library_utils]
*)

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
(** Add a new item to the type environment. Does not overwrite existing, but shadows *)

val set_raw : t -> string -> info list -> t
(** Overwrite the existing items bound to a name *)

val mem : t -> string -> bool
val iter : t -> (info list -> unit) -> unit

val nearest_ident : t -> string -> string option
(** The nearest identifier by edit distance, capped at edit distance 3 (if one exists) *)
