(** Type environments used during typechecking. Maps from strings to function or
    variable information *)

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

val block_name : originblock -> string

(** Information available for each variable *)
type varinfo =
  {origin: originblock; global: bool; readonly: bool; location: Location_span.t}

type info =
  { type_: UnsizedType.t
  ; kind:
      [ `Variable of varinfo
      | `UserDeclared of Location_span.t
      | `StanMath
      | `UserDefined of Location_span.t ] }

val location : info -> Location_span.t option

type t

val stan_math_environment : t
(** A type environment which contains the Stan math library functions *)

val find : t -> string -> info list

val add :
     t
  -> string
  -> Middle.UnsizedType.t
  -> [ `Variable of varinfo
     | `UserDeclared of Location_span.t
     | `StanMath
     | `UserDefined of Location_span.t ]
  -> t
(** Add a new item to the type environment. Does not overwrite existing, but
    shadows *)

val set_raw : t -> string -> info list -> t
(** Overwrite the existing items bound to a name *)

val mem : t -> string -> bool
val iteri : t -> (string -> info list -> unit) -> unit

val nearest_ident : t -> string -> (string * Location_span.t option list) option
(** The nearest identifier by edit distance, capped at edit distance 3 (if one
    exists) *)
