(** MIR statements. A statement pattern has separate type parameters for its
    expressions and its child statements. An analysis can then replace each
    child statement with a label. *)

module Pattern : sig
  (** A statement with expressions of type ['a] and children of type ['b]. *)
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
  [@@deriving sexp_of, map, fold]

  and 'e lvalue = 'e lbase * 'e Index.t list [@@deriving sexp_of, map, fold]

  and 'e lbase = LVariable of string | LTupleProjection of 'e lvalue * int
  [@@deriving sexp_of, map, fold]

  and 'a decl_init = Uninit | Default | Assign of 'a
  [@@deriving sexp_of, map, fold]

  val fold_map :
       ('a -> 'b -> 'a)
    -> ('a -> 'c -> 'a * 'd)
    -> 'a
    -> ('b, 'c) t
    -> 'a * ('b, 'd) t
  (** Folds the first function over the expressions. Folds the second function
      over the children and replaces each child of type ['c] with a ['d]. *)

  val pp : 'a Fmt.t -> 'b Fmt.t -> ('a, 'b) t Fmt.t
  (** Prints expressions with the first printer and children with the second.
      For a [For] whose body is label [8], an integer printer gives
      [for(n in 1:N) 8]. *)
end

(** A recursive statement. Its expressions have metadata of type ['a], and the
    statement has metadata of type ['b]. *)
type ('a, 'b) t = {pattern: ('a Expr.t, ('a, 'b) t) Pattern.t; meta: 'b}
[@@deriving sexp_of]

val pp : ('a, 'b) t Fmt.t

val rewrite_bottom_up :
     f:('a Expr.t -> 'a Expr.t)
  -> g:(('a, 'b) t -> ('a, 'b) t)
  -> ('a, 'b) t
  -> ('a, 'b) t
(** Rewrites expressions with [f] and statements with [g], from the bottom up.
    Each callback gets a node whose children are already rewritten. *)

(** Recursive statements with typed expressions and source locations. *)
module Located : sig
  module Meta : sig
    type t = Location_span.t

    val empty : t
  end

  type nonrec t = (Expr.Typed.Meta.t, (Meta.t[@sexp.opaque])) t
  [@@deriving sexp_of]

  val pp : t Fmt.t

  (** A statement with a source location. Its children are labels. *)
  module Non_recursive : sig
    type t = {pattern: (Expr.Typed.t, int) Pattern.t; meta: Meta.t}
  end
end

(** Recursive statements with an integer label as metadata. *)
module Numbered : sig
  module Meta : sig
    type t = int

    val empty : t
    val from_int : int -> t
  end

  type nonrec t = (Expr.Typed.Meta.t, Meta.t) t

  val pp : t Fmt.t
end

(** Helpers to build statements and to query assignment destinations. *)
module Helpers : sig
  val temp_vars :
    Expr.Typed.t list -> Located.t list * Expr.Typed.t list * (unit -> unit)

  val ensure_var :
    (Expr.Typed.t -> 'a -> Located.t) -> Expr.Typed.t -> 'a -> Located.t

  val internal_nrfunapp :
    'a Expr.t Internal_fun.t -> 'a Expr.t list -> 'b -> ('a, 'b) t

  val contains_fn_kind :
    ('a Expr.t Fun_kind.t -> bool) -> ?init:bool -> ('a, 'b) t -> bool

  val mk_for :
    Expr.Typed.t -> (Expr.Typed.t -> Located.t) -> Location_span.t -> Located.t

  val mk_nested_for :
       Expr.Typed.t list
    -> (Expr.Typed.t list -> Located.t)
    -> Location_span.t
    -> Located.t

  val for_each :
    (Expr.Typed.t -> Located.t) -> Expr.Typed.t -> Location_span.t -> Located.t

  val get_lhs_name : 'a Pattern.lvalue -> string
  (** The destination name with tuple fields and without subscripts. For
      [x[i].2[j]], the result is ["x.2"]. *)

  val lvariable : string -> 'e Pattern.lvalue
  val lvalue_of_expr_opt : 'e Expr.t -> 'e Expr.t Pattern.lvalue option

  val expr_of_lvalue : 'e Expr.t Pattern.lvalue -> meta:'e -> 'e Expr.t
  (** Builds an expression for the destination. Each new node gets [meta], so
      the types of the inner nodes can be wrong. *)

  val map_lhs_variable :
    f:(string -> string) -> 'e Pattern.lvalue -> 'e Pattern.lvalue

  val lhs_indices : 'e Pattern.lvalue -> 'e Index.t list

  val lhs_variable : 'e Pattern.lvalue -> string
  (** The base variable name, without tuple fields or subscripts. For
      [x[i].2[j]], the result is ["x"]. *)

  val lvalue_base_reference : 'e Pattern.lvalue -> 'e Pattern.lvalue
  (** Removes all subscripts and keeps all tuple fields.
      {@text[
        x[1, 2][3]          -> x
        x.1[1, 2].2[3].3    -> x.1.2.3
        x.1.2[1, 2][3].3    -> x.1.2.3
      ]} *)
end
