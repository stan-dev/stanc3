(** MIR statement representations and traversal helpers. A statement pattern
    separates expressions from child statements, allowing analyses to replace
    child statements with labels. Recursive statements retain their child bodies
    and attach metadata to each statement. *)

module Pattern : sig
  (** A statement with expressions of type ['a] and children of type ['b]. A
      [For] pattern can therefore hold a recursive body or the label used to
      locate the body in a statement map. *)
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
  (** Visits expressions with the first callback and replaces child statements
      with the second callback, carrying an accumulator through both. Expression
      values are retained; child values change from ['c] to ['d]. *)

  val pp : 'a Fmt.t -> 'b Fmt.t -> ('a, 'b) t Fmt.t
  (** Prints expressions with the first printer and child statements with the
      second. For a [For] pattern whose body is label [8], using an integer
      child printer gives [for(n in 1:N) 8]. The body is displayed as a
      reference rather than expanded into its statements. *)
end

(** A recursive statement with a pattern and metadata. Expression metadata has
    type ['a]; statement metadata has type ['b]. Children in [pattern] have the
    same recursive statement type. *)
type ('a, 'b) t = {pattern: ('a Expr.t, ('a, 'b) t) Pattern.t; meta: 'b}
[@@deriving sexp_of]

val pp : ('a, 'b) t Fmt.t

val rewrite_bottom_up :
     f:('a Expr.t -> 'a Expr.t)
  -> g:(('a, 'b) t -> ('a, 'b) t)
  -> ('a, 'b) t
  -> ('a, 'b) t
(** Rewrites expressions with [f] and statements with [g], visiting children
    before their parents. Each callback receives a node whose children have
    already been rewritten. For [x = a + 1], replacing [a] with [b] through [f]
    gives [x = b + 1] before [g] receives the assignment. *)

(** Recursive statements with typed expressions and source-location metadata.
    Dependency analysis retains the locations for warnings. *)
module Located : sig
  module Meta : sig
    type t = Location_span.t

    val empty : t
  end

  type nonrec t = (Expr.Typed.Meta.t, (Meta.t[@sexp.opaque])) t
  [@@deriving sexp_of]

  val pp : t Fmt.t

  (** Statement records with source locations and child labels. For a loop whose
      body has label [8], the pattern stores [8] in [For.body]. *)
  module Non_recursive : sig
    type t = {pattern: (Expr.Typed.t, int) Pattern.t; meta: Meta.t}
  end
end

(** Recursive statements with integer statement metadata. Children retain their
    full bodies; the integer identifies the containing statement. *)
module Numbered : sig
  module Meta : sig
    type t = int

    val empty : t
    val from_int : int -> t
  end

  type nonrec t = (Expr.Typed.Meta.t, Meta.t) t

  val pp : t Fmt.t
end

(** Statement construction and assignment-destination queries. Destination
    queries can retain tuple fields or select only the base variable name,
    depending on the facts an analysis needs. *)
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
  (** Returns the destination name with tuple fields, omitting subscripts. For
      [x[i].2[j]], the result is ["x.2"]. *)

  val lvariable : string -> 'e Pattern.lvalue
  val lvalue_of_expr_opt : 'e Expr.t -> 'e Expr.t Pattern.lvalue option

  val expr_of_lvalue : 'e Expr.t Pattern.lvalue -> meta:'e -> 'e Expr.t
  (** Builds an expression for the destination using [meta] at every generated
      node. For [x[i]], the expression contains a base [Var "x"] and an index
      step. Reusing the outer metadata means generated inner-node types can be
      incorrect; callers must account for those types before typed rewrites. *)

  val map_lhs_variable :
    f:(string -> string) -> 'e Pattern.lvalue -> 'e Pattern.lvalue

  val lhs_indices : 'e Pattern.lvalue -> 'e Index.t list

  val lhs_variable : 'e Pattern.lvalue -> string
  (** Returns the base variable name without tuple fields or subscripts. For
      [x[i].2[j]], the result is ["x"]. Access collection stores the selected
      fields and subscripts separately in the access path. *)

  val lvalue_base_reference : 'e Pattern.lvalue -> 'e Pattern.lvalue
  (** Removes all subscripts and retains all tuple-field projections. The
      following examples show the retained reference after element selections
      are removed:
      {@text[
        x[1, 2][3]          -> x
        x.1[1, 2].2[3].3    -> x.1.2.3
        x.1.2[1, 2][3].3    -> x.1.2.3
      ]} *)
end
