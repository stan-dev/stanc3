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

  val map_accum_left :
    f:('state -> 'a -> 'b * 'state) -> init:'state -> 'a t -> 'b t * 'state

  val map_accum_right :
    f:('state -> 'a -> 'b * 'state) -> init:'state -> 'a t -> 'b t * 'state
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
      ; label: Int_label.t }
    [@@deriving compare, create, sexp, hash]

    include Meta.S with type t := t

    val adlevel : t -> UnsizedType.autodifftype
    val type_ : t -> UnsizedType.t
    val loc : t -> Location_span.t
    val label : t -> Int_label.t
  end

  include Specialized.S with module Meta := Meta and type t = Meta.t Fixed.t

  val type_of : t -> UnsizedType.t
  val loc_of : t -> Location_span.t
  val adlevel_of : t -> UnsizedType.autodifftype
  val label_of : t -> Int_label.t
  val label : ?init:int -> Typed.t -> t
  val associate : ?init:t Int_label.Map.t -> t -> t Int_label.Map.t
  val associate_index : t Int_label.Map.t -> t index -> t Int_label.Map.t
end

val var : 'a -> string -> 'a Fixed.t

(* == Literals ============================================================== *)
val lit : 'a -> litType -> string -> 'a Fixed.t
val lit_int : 'a -> int -> 'a Fixed.t
val lit_real : 'a -> float -> 'a Fixed.t
val lit_string : 'a -> string -> 'a Fixed.t
val is_lit : ?type_:litType -> 'a Fixed.t -> bool
val int_of_lit : 'a Fixed.t -> int option
val real_of_lit : 'a Fixed.t -> float option
val string_of_lit : 'a Fixed.t -> string option

(* == Logical =============================================================== *)
val and_ : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val or_ : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t

(* == Indexed expressions =================================================== *)
val indexed : 'a -> 'a Fixed.t -> 'a Fixed.t index list -> 'a Fixed.t
val index_all : 'a -> 'a Fixed.t -> 'a Fixed.t
val index_single : 'a -> 'a Fixed.t -> idx:'a Fixed.t -> 'a Fixed.t
val index_multi : 'a -> 'a Fixed.t -> idx:'a Fixed.t -> 'a Fixed.t
val index_upfrom : 'a -> 'a Fixed.t -> idx:'a Fixed.t -> 'a Fixed.t

val index_between :
  'a -> 'a Fixed.t -> lower:'a Fixed.t -> upper:'a Fixed.t -> 'a Fixed.t

val index_bounds : 'a index -> 'a list
val indices_of : 'a Fixed.t -> 'a Fixed.t index list

(* == Ternary If ============================================================ *)
val if_ : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t

(* == Function application ================================================== *)
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

(* == Binary operations ===================================================== *)
val apply_binop :
  'a -> Operator.t -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t option

val binop : 'a -> Operator.t -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t

(* -- Plus ------------------------------------------------------------------ *)

val plus : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t

(* -- Minus ----------------------------------------------------------------- *)
val minus : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t

(* -- Times ----------------------------------------------------------------- *)
val simplify_times_opt : Typed.Meta.t -> Typed.t -> Typed.t -> Typed.t option
val times : Typed.Meta.t -> Typed.t -> Typed.t -> Typed.t

(* -- Divide ---------------------------------------------------------------- *)
val divide : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t

(* -- Pow ------------------------------------------------------------------- *)
val simplify_pow_opt : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t option
val pow : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t

(* -- Modulo ---------------------------------------------------------------- *)
val modulo : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t

(* -- Comparison ------------------------------------------------------------ *)
val eq : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val neq : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val gt : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val gteq : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val lt : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val lteq : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t

(* -- Logical --------------------------------------------------------------- *)
val logical_and : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t
val logical_or : 'a -> 'a Fixed.t -> 'a Fixed.t -> 'a Fixed.t

(* == Unary operations ====================================================== *)
val apply_unop : 'a -> Operator.t -> 'a Fixed.t -> 'a Fixed.t option
val unop : 'a -> Operator.t -> 'a Fixed.t -> 'a Fixed.t
val transpose : 'a -> 'a Fixed.t -> 'a Fixed.t
val logical_not : 'a -> 'a Fixed.t -> 'a Fixed.t
val negate : 'a -> 'a Fixed.t -> 'a Fixed.t
val positive : 'a -> 'a Fixed.t -> 'a Fixed.t

(* == General derived helpers =============================================== *)
val incr : 'a Fixed.t -> 'a Fixed.t
val decr : 'a Fixed.t -> 'a Fixed.t

(* == Constants ============================================================= *)
val zero : 'a -> 'a Fixed.t
val loop_bottom : 'a -> 'a Fixed.t
val sqrt2 : 'a -> 'a Fixed.t

(* == StanLib smart constructors ============================================ *)
(* -- Log ------------------------------------------------------------------- *)
val simplify_log_opt : Typed.Meta.t -> Typed.t -> Typed.t option
val log : Typed.Meta.t -> Typed.t -> Typed.t

(* -- Sum ------------------------------------------------------------------- *)
val simplify_sum_opt : Typed.Meta.t -> Typed.t -> Typed.t option
val sum : Typed.Meta.t -> Typed.t -> Typed.t

(* -- Square ---------------------------------------------------------------- *)
val simplify_square_opt : Typed.Meta.t -> Typed.t -> Typed.t option
val square : Typed.Meta.t -> Typed.t -> Typed.t

(* -- Square Root ----------------------------------------------------------- *)
val simplify_sqrt_opt : Typed.Meta.t -> Typed.t -> Typed.t option
val sqrt : Typed.Meta.t -> Typed.t -> Typed.t

(* -- Inv ------------------------------------------------------------------- *)
val simplify_inv_opt : Typed.Meta.t -> Typed.t -> Typed.t option
val inv : Typed.Meta.t -> Typed.t -> Typed.t

(* -- Trace ----------------------------------------------------------------- *)
val simplify_trace_opt : Typed.Meta.t -> Typed.t -> Typed.t option
val trace : Typed.Meta.t -> Typed.t -> Typed.t

(* -- Dot product ----------------------------------------------------------- *)
val simplify_dot_product_opt :
  Typed.Meta.t -> Typed.t -> Typed.t -> Typed.t option

val dot_product : Typed.Meta.t -> Typed.t -> Typed.t -> Typed.t

(* -- Rows dot product ------------------------------------------------------ *)
val simplify_rows_dot_product_opt :
  Typed.Meta.t -> Typed.t -> Typed.t -> Typed.t option

val rows_dot_product : Typed.Meta.t -> Typed.t -> Typed.t -> Typed.t

(* -- Columns dot product --------------------------------------------------- *)
val simplify_columns_dot_product_opt :
  Typed.Meta.t -> Typed.t -> Typed.t -> Typed.t option

val columns_dot_product : Typed.Meta.t -> Typed.t -> Typed.t -> Typed.t

(* == Transformations for distributions ===================================== *)
val lpdf_glm_lpdf :
  (Typed.t -> Typed.t -> Typed.t -> 'a) -> Typed.t -> 'a option

val lpdf_trans_glm_lpdf :
  link:string -> (Typed.t -> Typed.t -> Typed.t -> 'a) -> Typed.t -> 'a option

val lpdf_trans_lpdf :
  link:string -> ('a Fixed.t -> 'b) -> 'a Fixed.t -> 'b option

val rng_trans_rng :
  link:string -> ('a Fixed.t -> 'b) -> 'a Fixed.t -> 'b option


(* == Partial evaluation  =================================================== *)

val eval : ?env:Typed.t String.Map.t -> Typed.t -> Typed.t
