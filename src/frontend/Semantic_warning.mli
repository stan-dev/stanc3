open Middle

type t

val pp : Format.formatter -> t -> unit
val location : t -> location_span

val warn_autodiff_level :
  location_span -> string -> autodifftype -> autodifftype -> t
