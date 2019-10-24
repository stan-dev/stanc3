open Middle

type t

val pp : Format.formatter -> t -> unit
val location : t -> Location_span.t

val warn_autodiff_level :
     Location_span.t
  -> string
  -> UnsizedType.autodifftype
  -> UnsizedType.autodifftype
  -> t
