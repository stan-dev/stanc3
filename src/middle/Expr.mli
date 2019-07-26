open Core_kernel
open Common
module Pattern : Pattern.S with type 'a t = 'a Mir_pattern.expr
module Fixed : Fix.S with module Pattern := Pattern

module NoMeta : sig
  module Meta : sig
    type t = unit [@@deriving compare, sexp, hash]

    include Pretty.S with type t := t
  end

  include Specialized.S with module Meta := Meta and type t = Meta.t Fixed.t

  val remove_meta : 'a Fixed.t -> t
end

module Typed : sig
  module Meta : sig
    type t =
      { type_: UnsizedType.t
      ; loc: Location_span.t sexp_opaque [@compare.ignore]
      ; adlevel: UnsizedType.autodifftype }
    [@@deriving compare, create, sexp, hash]

    include Pretty.S with type t := t

    val adlevel : t -> UnsizedType.autodifftype
    val type_ : t -> UnsizedType.t
    val loc : t -> Location_span.t
  end

  include Specialized.S with module Meta := Meta and type t = Meta.t Fixed.t

  val type_of : t -> UnsizedType.t
  val loc_of : t -> Location_span.t
  val adlevel_of : t -> UnsizedType.autodifftype
end

module Labelled : sig
  module Meta : sig
    type t =
      { type_: UnsizedType.t
      ; loc: Location_span.t sexp_opaque [@compare.ignore]
      ; adlevel: UnsizedType.autodifftype
      ; label: Label.t [@compare.ignore] }
    [@@deriving compare, create, sexp, hash]

    include Pretty.S with type t := t

    val adlevel : t -> UnsizedType.autodifftype
    val type_ : t -> UnsizedType.t
    val loc : t -> Location_span.t
    val label : t -> Label.t
  end

  include Specialized.S with module Meta := Meta and type t = Meta.t Fixed.t

  val type_of : t -> UnsizedType.t
  val loc_of : t -> Location_span.t
  val adlevel_of : t -> UnsizedType.autodifftype
  val label_of : t -> Label.t
  val label : ?init:int -> Typed.t -> t
  val associate : ?init:t Label.Map.t -> t -> t Label.Map.t
end

include Fix.S with module Pattern := Pattern

type 'a index = 'a Mir_pattern.index =
  | All
  | Single of 'a
  | Upfrom of 'a
  | Between of 'a * 'a
  | MultiIndex of 'a
[@@deriving sexp, hash, map, compare, fold]

val var : 'a -> string -> 'a t
val lit_int : 'a -> int -> 'a t
val lit_real : 'a -> float -> 'a t
val lit_string : 'a -> string -> 'a t
val if_ : 'a -> 'a t -> 'a t -> 'a t -> 'a t
val and_ : 'a -> 'a t -> 'a t -> 'a t
val or_ : 'a -> 'a t -> 'a t -> 'a t
val all : 'a t index
val single : 'a t -> 'a t index
