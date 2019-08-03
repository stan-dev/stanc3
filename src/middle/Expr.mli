open Core_kernel
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
val pp_indexed : 'a Fmt.t -> Format.formatter -> string * 'a index list -> unit

module Fixed : sig
  module Pattern : sig
    type 'a t = 'a Mir_pattern.expr =
      | Var of string
      | Lit of litType * string
      | FunApp of Fun_kind.t * string * 'a list
      | TernaryIf of 'a * 'a * 'a
      | EAnd of 'a * 'a
      | EOr of 'a * 'a
      | Indexed of 'a * 'a index list
    [@@deriving sexp, hash, compare]

    include Pattern.S with type 'a t := 'a t
  end

  include Fix.S with module Pattern := Pattern
end

module NoMeta : sig
  module Meta : sig
    type t = unit [@@deriving compare, sexp, hash]

    include Meta.S with type t := unit
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

    include Meta.S with type t := t

    val empty : t
    val adlevel : t -> UnsizedType.autodifftype
    val type_ : t -> UnsizedType.t
    val loc : t -> Location_span.t
    val with_type : UnsizedType.t -> t -> t
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
      ; label: Label.t }
    [@@deriving compare, create, sexp, hash]

    include Meta.S with type t := t

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
  val associate_index : t Label.Map.t -> t index -> t Label.Map.t
end

val proj : 'a Fixed.t -> 'a * 'a Fixed.t Fixed.Pattern.t
val meta : 'a Fixed.t -> 'a
val pattern : 'a Fixed.t -> 'a Fixed.t Fixed.Pattern.t
val inj : 'a * 'a Fixed.t Fixed.Pattern.t -> 'a Fixed.t
val fix : 'a -> 'a Fixed.t Fixed.Pattern.t -> 'a Fixed.t
val var : 'a -> string -> 'a Fixed.t
val lit : 'a -> litType -> string -> 'a Fixed.t
val lit_int : 'a -> int -> 'a Fixed.t
val lit_real : 'a -> float -> 'a Fixed.t
val lit_string : 'a -> string -> 'a Fixed.t
val is_lit : ?type_:litType -> 'a Fixed.t -> bool
val and_ : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val or_ : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val indexed : 'a -> 'a Fixed.t -> 'a Fixed.t index list -> 'a Fixed.t
val index_all : 'a -> 'a Fixed.t -> 'a Fixed.t
val index_single : 'a -> 'a Fixed.t -> idx:'a Fixed.t -> 'a Fixed.t
val index_multi : 'a -> 'a Fixed.t -> idx:'a Fixed.t -> 'a Fixed.t
val index_upfrom : 'a -> 'a Fixed.t -> idx:'a Fixed.t -> 'a Fixed.t

val index_between :
  'a -> 'a Fixed.t -> lower:'a Fixed.t -> upper:'a Fixed.t -> 'a Fixed.t

val index_bounds : 'a index -> 'a list
val indices_of : 'a Fixed.t -> 'a Fixed.t index list
val if_ : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val fun_app : 'a -> Fun_kind.t -> string -> 'a Fixed.t list -> 'a Fixed.t
val internal_fun : 'a -> Internal_fun.t -> 'a Fixed.t list -> 'a Fixed.t
val user_fun : 'a -> string -> 'a Fixed.t list -> 'a Fixed.t
val stanlib_fun : 'a -> string -> 'a Fixed.t list -> 'a Fixed.t
val is_fun : ?kind:Fun_kind.t -> ?name:string -> 'a Fixed.t -> bool
val is_internal_fun : ?fn:Internal_fun.t -> 'a Fixed.t -> bool
val is_operator : ?op:Operator.t -> 'a Fixed.t -> bool

val contains_fun_algebra :
  ?kind:Fun_kind.t -> ?name:string -> ('a, bool) Fixed.algebra

val contains_fun : ?kind:Fun_kind.t -> ?name:string -> 'a Fixed.t -> bool
val contains_operator : ?op:Operator.t -> 'a Fixed.t -> bool
val contains_internal_fun : ?fn:Internal_fun.t -> 'a Fixed.t -> bool
val binop : 'a -> Operator.t -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val plus : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val minus : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val times : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val divide : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val pow : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val modulo : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val eq : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val neq : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val gt : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val gteq : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val lt : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val lteq : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val l_and : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val l_or : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val unop : 'a -> Operator.t -> 'a Fixed.t -> 'a Fixed.t
val transpose : 'a -> 'a Fixed.t -> 'a Fixed.t
val l_not : 'a -> 'a Fixed.t -> 'a Fixed.t
val negate : 'a -> 'a Fixed.t -> 'a Fixed.t
val incr : 'a Fixed.t -> 'a Fixed.t
val decr : 'a Fixed.t -> 'a Fixed.t
val zero : Typed.t
val loop_bottom : Typed.t

module Bernoulli : sig 
  val logit_glm_lpmf : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t  
  val logit_glm_lpmf_checked : Typed.Meta.t -> Typed.t -> Typed.t -> Typed.t -> Typed.t -> Typed.t option
  
  
  val logit_lpmf : Typed.Meta.t -> Typed.t -> Typed.t -> Typed.t
  val logit_lpmf_checked : Typed.Meta.t -> Typed.t -> Typed.t -> Typed.t option
  val logit_rng : 'a -> 'a Fixed.t -> 'a Fixed.t

  val lpmf : Typed.Meta.t -> Typed.t -> Typed.t -> Typed.t
  val cdf : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
  val lcdf : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
  val lccdf : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
  val rng : 'a -> 'a Fixed.t -> 'a Fixed.t 


end 