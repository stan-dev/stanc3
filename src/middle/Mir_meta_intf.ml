open Core_kernel
open Common

module type NoMeta = sig
  type t = 
    unit 
    [@@deriving compare, sexp, hash]
  include Meta.S with type t := unit
end

module type Typed = sig 
    type t =
      { type_: UnsizedType.t
      ; loc: Location_span.t sexp_opaque [@compare.ignore]
      ; adlevel: UnsizedType.autodifftype }
    [@@deriving compare, create, sexp, hash]
    include Meta.S with type t := t
    val empty : t
    val adlevel : t -> UnsizedType.autodifftype
    val type_ : t -> UnsizedType.t 
    val loc : t -> Location_span.t
end 

module type Labelled = sig 
    type t =
      { type_: UnsizedType.t
      ; loc: Location_span.t sexp_opaque [@compare.ignore]
      ; adlevel: UnsizedType.autodifftype 
      ; label : Label.t}
    [@@deriving compare, create, sexp, hash]
    include Meta.S with type t := t
    val adlevel : t -> UnsizedType.autodifftype
    val type_ : t -> UnsizedType.t 
    val loc : t -> Location_span.t
    val label : t -> Label.t
end

module type S = sig 
    module NoMeta : NoMeta 
    module Typed : Typed 
    module Labelled : Labelled
end 