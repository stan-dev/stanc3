(** MIR types and modules corresponding to the statements of the language *)

module Fixed : sig
  module Pattern : sig
    type ('a, 'b) t =
      | Assignment of 'a lvalue * UnsizedType.t * 'a
      | TargetPE of 'a
      | JacobianPE of 'a
      | NRFunApp of 'a Fun_kind.t * 'a list
      | Break
      | Continue
      | Return of 'a option
      | Skip
      | IfElse of 'a * 'b * 'b option
      | While of 'a * 'b
      | For of {loopvar: string; lower: 'a; upper: 'a; body: 'b}
      | Profile of string * 'b list
      | Block of 'b list
      | SList of 'b list
      | Decl of
          { decl_adtype: UnsizedType.autodifftype
          ; decl_id: string
          ; decl_type: 'a Type.t
          ; initialize: 'a decl_init }
    [@@deriving sexp, hash, compare, map, fold]

    and 'e lvalue = 'e lbase * 'e Index.t list
    [@@deriving sexp, hash, map, compare, fold]

    and 'e lbase = LVariable of string | LTupleProjection of 'e lvalue * int
    [@@deriving sexp, hash, map, compare, fold]

    and 'a decl_init = Uninit | Default | Assign of 'a
    [@@deriving sexp, hash, map, fold, compare]
  end

  (** The "two-level" type for statements in the MIR. This corresponds to what the AST calls
      [Frontend.Ast.statement_with]  *)
  type ('a, 'b) t = {pattern: ('a Expr.Fixed.t, ('a, 'b) t) Pattern.t; meta: 'b}
  [@@deriving compare, hash, sexp]

  val pp : ('a, 'b) t Fmt.t

  val rewrite_bottom_up :
       f:('a Expr.Fixed.t -> 'a Expr.Fixed.t)
    -> g:(('a, 'b) t -> ('a, 'b) t)
    -> ('a, 'b) t
    -> ('a, 'b) t
  (** [rewrite_bottom_up] specializes [fold] so that the result type
  ['r1] is equal to the type of the nested fixed-point type
  i.e. ['r1 = 'a First.t] and the result type ['r2] is equal to the top-level
  fixed-point type i.e. ['r2 = ('a,'b) t].

  This also means that the function [f] can be written with our nested
  fixed-point type  ['a First.t] as its argument and [g] can be written with
  [('a,'b) t] as its argument.
  *)
end

module Located : sig
  module Meta : sig
    type t = (Location_span.t[@sexp.opaque] [@compare.ignore])
    [@@deriving compare, sexp, hash]

    val empty : t
  end

  type t = (Expr.Typed.Meta.t, (Meta.t[@sexp.opaque] [@compare.ignore])) Fixed.t
  [@@deriving compare, sexp, hash]

  val loc_of : t -> Location_span.t
  val pp : t Fmt.t

  module Non_recursive : sig
    type t =
      { pattern: (Expr.Typed.t, int) Fixed.Pattern.t
      ; meta: (Meta.t[@sexp.opaque] [@compare.ignore]) }
    [@@deriving compare, sexp, hash]
  end
end

module Numbered : sig
  module Meta : sig
    type t = (int[@sexp.opaque] [@compare.ignore])
    [@@deriving compare, sexp, hash]

    val empty : t
    val from_int : int -> t
  end

  type t = (Expr.Typed.Meta.t, (Meta.t[@sexp.opaque] [@compare.ignore])) Fixed.t
  [@@deriving compare, sexp, hash]

  val pp : t Fmt.t
end

module Helpers : sig
  val temp_vars :
    Expr.Typed.t list -> Located.t list * Expr.Typed.t list * (unit -> unit)

  val ensure_var :
    (Expr.Typed.t -> 'a -> Located.t) -> Expr.Typed.t -> 'a -> Located.t

  val internal_nrfunapp :
       'a Expr.Fixed.t Internal_fun.t
    -> 'a Expr.Fixed.t list
    -> 'b
    -> ('a, 'b) Fixed.t

  val contains_fn_kind :
       ('a Expr.Fixed.t Fun_kind.t -> bool)
    -> ?init:bool
    -> ('a, 'b) Fixed.t
    -> bool

  val mk_for :
    Expr.Typed.t -> (Expr.Typed.t -> Located.t) -> Location_span.t -> Located.t

  val mk_nested_for :
       Expr.Typed.t list
    -> (Expr.Typed.t list -> Located.t)
    -> Location_span.t
    -> Located.t

  val mk_for_iteratee :
       Expr.Typed.t
    -> (Expr.Typed.t -> Located.t)
    -> Expr.Typed.t
    -> Location_span.t
    -> Located.t

  val for_each :
    (Expr.Typed.t -> Located.t) -> Expr.Typed.t -> Location_span.t -> Located.t

  val for_scalar :
       Expr.Typed.t SizedType.t
    -> (Expr.Typed.t SizedType.t -> Expr.Typed.t -> Located.t)
    -> Expr.Typed.t
    -> Location_span.t
    -> Located.t

  val for_scalar_inv :
       Expr.Typed.t SizedType.t
    -> (Expr.Typed.t SizedType.t -> Expr.Typed.t -> Located.t)
    -> Expr.Typed.t
    -> Location_span.t
    -> Located.t

  val assign_indexed :
       UnsizedType.t
    -> 'b Expr.Fixed.t Fixed.Pattern.lvalue
    -> 'a
    -> ('b Expr.Fixed.t -> 'b Expr.Fixed.t)
    -> 'b Expr.Fixed.t
    -> ('b, 'a) Fixed.t

  val get_lhs_name : 'a Fixed.Pattern.lvalue -> string
  (** The name of the lhs.
  This adds "." and an index to tuple projections *)

  val lvariable : string -> 'e Fixed.Pattern.lvalue

  val lvalue_of_expr_opt :
    'e Expr.Fixed.t -> 'e Expr.Fixed.t Fixed.Pattern.lvalue option

  val expr_of_lvalue :
    'e Expr.Fixed.t Fixed.Pattern.lvalue -> meta:'e -> 'e Expr.Fixed.t

  val map_lhs_variable :
    f:(string -> string) -> 'e Fixed.Pattern.lvalue -> 'e Fixed.Pattern.lvalue

  val lhs_indices : 'e Fixed.Pattern.lvalue -> 'e Index.t list

  val lhs_variable : 'e Fixed.Pattern.lvalue -> string
  (** This gets the innermost name of the variable.
  It differs from [get_lhs_name] in that tuple
  projections do not add their indices here. *)

  val lvalue_base_reference : 'e Fixed.Pattern.lvalue -> 'e Fixed.Pattern.lvalue
  (** Reduce an lvalue down to its "base reference", which is a variable with maximum tuple indices after it.
     For example:
     {[x[1,2][3] -> x
     x.1[1,2].2[3].3 -> x.1
     x.1.2[1,2][3].3 -> x.1.2]}
  *)
end
