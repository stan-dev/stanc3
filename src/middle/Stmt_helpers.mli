open Core_kernel
open Stmt

val free_vars : ('a, 'b) Fixed.t -> String.Set.t
val break : 'b -> ('a, 'b) Fixed.t
val continue : 'b -> ('a, 'b) Fixed.t
val skip : 'b -> ('a, 'b) Fixed.t
val target_pe : 'b -> 'a Expr.Fixed.t -> ('a, 'b) Fixed.t
val return_ : 'b -> 'a Expr.Fixed.t -> ('a, 'b) Fixed.t
val return_void : 'b -> ('a, 'b) Fixed.t
val is_return : ('a, 'b) Fixed.t -> bool
val return_value_opt : ('a, 'b) Fixed.t -> 'a Expr.Fixed.t option

val assign :
     'b
  -> string
  -> ?idxs:'a Expr.Fixed.t Expr.index list
  -> 'a Expr.Fixed.t
  -> ('a, 'b) Fixed.t

val is_assignment : ('a, 'b) Fixed.t -> bool
val block : 'b -> ('a, 'b) Fixed.t list -> ('a, 'b) Fixed.t
val is_block : ('a, 'b) Fixed.t -> bool
val lift_to_block : ('a, 'b) Fixed.t -> ('a, 'b) Fixed.t
val block_statements : ('a, 'b) Fixed.t -> ('a, 'b) Fixed.t list
val slist : 'b -> ('a, 'b) Fixed.t list -> ('a, 'b) Fixed.t
val while_ : 'b -> 'a Expr.Fixed.t -> ('a, 'b) Fixed.t -> ('a, 'b) Fixed.t
val is_while : ('a, 'b) Fixed.t -> bool

val for_ :
     'b
  -> string
  -> 'a Expr.Fixed.t
  -> 'a Expr.Fixed.t
  -> ('a, 'b) Fixed.t
  -> ('a, 'b) Fixed.t

val is_for : ('a, 'b) Fixed.t -> bool
val body_opt : ('a, 'b) Fixed.t -> ('a, 'b) Fixed.t option

val if_ :
     'b
  -> 'a Expr.Fixed.t
  -> ('a, 'b) Fixed.t
  -> ('a, 'b) Fixed.t option
  -> ('a, 'b) Fixed.t

val is_if_else : ('a, 'b) Fixed.t -> bool

val declare_sized :
     'b
  -> UnsizedType.autodifftype
  -> string
  -> 'a Expr.Fixed.t SizedType.t
  -> ('a, 'b) Fixed.t

val declare_unsized :
  'b -> UnsizedType.autodifftype -> string -> UnsizedType.t -> ('a, 'b) Fixed.t

val is_decl : ('a, 'b) Fixed.t -> bool

val nrfun_app :
  'b -> Fun_kind.t -> string -> 'a Expr.Fixed.t list -> ('a, 'b) Fixed.t

val stanlib_nrfun : 'b -> string -> 'a Expr.Fixed.t list -> ('a, 'b) Fixed.t

val internal_nrfun :
  'b -> Internal_fun.t -> 'a Expr.Fixed.t list -> ('a, 'b) Fixed.t

val user_nrfun : 'b -> string -> 'a Expr.Fixed.t list -> ('a, 'b) Fixed.t
val is_nrfun : ?kind:Fun_kind.t -> ?name:string -> ('a, 'b) Fixed.t -> bool
val is_internal_nrfun : ?fn:Internal_fun.t -> ('a, 'b) Fixed.t -> bool
val contains_fun : ?kind:Fun_kind.t -> ?name:string -> ('a, 'b) Fixed.t -> bool
val contains_operator : ?op:Operator.t -> ('a, 'b) Fixed.t -> bool
val contains_internal_fun : ?fn:Internal_fun.t -> ('a, 'b) Fixed.t -> bool
