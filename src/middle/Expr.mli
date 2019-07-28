
open Common

type litType = Mir_pattern.litType = Int | Real | Str
[@@deriving sexp, hash, compare]

type 'a index = 'a Mir_pattern.index =
  | All
  | Single of 'a
  | Upfrom of 'a
  | Between of 'a * 'a
  | MultiIndex of 'a
[@@deriving sexp, hash, map, compare, fold]

val pp_index : 'a Fmt.t -> Format.formatter -> 'a index -> unit
val pp_indexed : 'a Fmt.t ->Format.formatter -> string * 'a index list -> unit
module Fixed  : sig 
  module Pattern : Pattern.S with type 'a t = 'a Mir_pattern.expr  
  include Fix.S with module Pattern := Pattern
end

module NoMeta : sig
  module Meta : Mir_meta_intf.NoMeta

  include Specialized.S with module Meta := Meta and type t = Meta.t Fixed.t

  val remove_meta : 'a Fixed.t -> t
end

module Typed : sig
  module Meta : Mir_meta_intf.Typed

  include Specialized.S with module Meta := Meta and type t = Meta.t Fixed.t

  val type_of : t -> UnsizedType.t
  val loc_of : t -> Location_span.t
  val adlevel_of : t -> UnsizedType.autodifftype
end

module Labelled : sig
  module Meta : Mir_meta_intf.Labelled

  include Specialized.S with module Meta := Meta and type t = Meta.t Fixed.t

  val type_of : t -> UnsizedType.t
  val loc_of : t -> Location_span.t
  val adlevel_of : t -> UnsizedType.autodifftype
  val label_of : t -> Label.t
  val label : ?init:int -> Typed.t -> t
  val associate : ?init:t Label.Map.t -> t -> t Label.Map.t
  val associate_index : t Label.Map.t -> t index -> t Label.Map.t
end


val var : 'a -> string -> 'a Fixed.t
val lit : 'a -> litType -> string -> 'a Fixed.t
val lit_int : 'a -> int -> 'a Fixed.t
val lit_real : 'a -> float -> 'a Fixed.t
val lit_string : 'a -> string -> 'a Fixed.t
val if_ : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val and_ : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val or_ : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val index_all : 'a Fixed.t index
val index_single : 'a Fixed.t -> 'a Fixed.t index
val index_multi : 'a Fixed.t -> 'a Fixed.t index
val index_upfrom : 'a Fixed.t -> 'a Fixed.t index
val index_between : 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t index
val indexed : 'a -> 'a Fixed.t -> 'a Fixed.t index list -> 'a Fixed.t
val index_bounds : 'a Fixed.t index -> 'a Fixed.t list 
val fun_app  : 'a -> Fun_kind.t -> string -> 'a Fixed.t list -> 'a Fixed.t 
val compiler_fun : 'a -> string -> 'a Fixed.t list -> 'a Fixed.t 
val user_fun : 'a -> string -> 'a Fixed.t list -> 'a Fixed.t 
val stanlib_fun : 'a -> string -> 'a Fixed.t list -> 'a Fixed.t
val binop : 'a -> Operator.t -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t