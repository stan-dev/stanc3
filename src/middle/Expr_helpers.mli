open Core_kernel
open Expr

val var : 'a -> string -> 'a Fixed.t
val lit : 'a -> litType -> string -> 'a Fixed.t
val lit_int : 'a -> int -> 'a Fixed.t
val lit_real : 'a -> float -> 'a Fixed.t
val lit_string : 'a -> string -> 'a Fixed.t
val is_lit : ?type_:litType -> 'a Fixed.t -> bool
val int_of_lit : 'a Fixed.t -> int option
val real_of_lit : 'a Fixed.t -> float option
val string_of_lit : 'a Fixed.t -> string option
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
val ternary_if : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val fun_app : 'a -> Fun_kind.t -> string -> 'a Fixed.t list -> 'a Fixed.t
val internal_fun : 'a -> Internal_fun.t -> 'a Fixed.t list -> 'a Fixed.t
val user_fun : 'a -> string -> 'a Fixed.t list -> 'a Fixed.t
val stanlib_fun : 'a -> string -> 'a Fixed.t list -> 'a Fixed.t
val is_fun : ?kind:Fun_kind.t -> ?name:string -> 'a Fixed.t -> bool
val is_internal_fun : ?fn:Internal_fun.t -> 'a Fixed.t -> bool
val is_operator : ?op:Operator.t -> 'a Fixed.t -> bool
val is_trivial : 'a Fixed.t -> bool
val free_vars_algebra : ('a, String.Set.t) Fixed.algebra
val free_vars : 'a Fixed.t -> String.Set.t

val contains_fun_algebra :
  ?kind:Fun_kind.t -> ?name:string -> ('a, bool) Fixed.algebra

val contains_fun : ?kind:Fun_kind.t -> ?name:string -> 'a Fixed.t -> bool
val contains_operator : ?op:Operator.t -> 'a Fixed.t -> bool
val contains_internal_fun : ?fn:Internal_fun.t -> 'a Fixed.t -> bool

val apply_binop :
  'a -> Operator.t -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t option

val binop : 'a -> Operator.t -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val plus : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val minus : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val times : Typed.Meta.t -> Typed.t -> Typed.t -> Typed.t
val divide : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val pow : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val modulo : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val eq : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val neq : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val gt : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val gteq : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val lt : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val lteq : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val logical_and : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val logical_or : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val apply_unop : 'a -> Operator.t -> 'a Fixed.t -> 'a Fixed.t option
val unop : 'a -> Operator.t -> 'a Fixed.t -> 'a Fixed.t
val transpose : 'a -> 'a Fixed.t -> 'a Fixed.t
val logical_not : 'a -> 'a Fixed.t -> 'a Fixed.t
val negate : 'a -> 'a Fixed.t -> 'a Fixed.t
val positive : 'a -> 'a Fixed.t -> 'a Fixed.t
val incr : 'a Fixed.t -> 'a Fixed.t
val decr : 'a Fixed.t -> 'a Fixed.t
val zero : 'a -> 'a Fixed.t
val loop_bottom : 'a -> 'a Fixed.t
val sqrt2 : 'a -> 'a Fixed.t
val log : Typed.Meta.t -> Typed.t -> Typed.t
val sum : Typed.Meta.t -> Typed.t -> Typed.t
val square : Typed.Meta.t -> Typed.t -> Typed.t
val sqrt : Typed.Meta.t -> Typed.t -> Typed.t
val inv : Typed.Meta.t -> Typed.t -> Typed.t
val trace : Typed.Meta.t -> Typed.t -> Typed.t
val dot_product : Typed.Meta.t -> Typed.t -> Typed.t -> Typed.t
val rows_dot_product : Typed.Meta.t -> Typed.t -> Typed.t -> Typed.t
val columns_dot_product : Typed.Meta.t -> Typed.t -> Typed.t -> Typed.t

module type Evaluable = sig
  module Meta : Common.Meta.S

  val type_of : Meta.t Fixed.t -> UnsizedType.t
  val adlevel_of : Meta.t Fixed.t -> UnsizedType.autodifftype
  val equal : Meta.t Fixed.t -> Meta.t Fixed.t -> bool
end

val eval :
     type_of:('a Fixed.t -> UnsizedType.t)
  -> adlevel_of:('a Fixed.t -> UnsizedType.autodifftype)
  -> equal:('a Fixed.t -> 'a Fixed.t -> bool)
  -> ?env:'a Fixed.t String.Map.t
  -> 'a Fixed.t
  -> 'a Fixed.t
