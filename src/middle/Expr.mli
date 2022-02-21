(** MIR types and modules corresponding to the expressions of the language *)

open Common

module Fixed : sig
  module Pattern : sig
    type litType = Int | Real | Imaginary | Str
    [@@deriving sexp, hash, compare]

    type 'a t =
      | Var of string
      | Lit of litType * string
      | FunApp of 'a Fun_kind.t * 'a list
      | TernaryIf of 'a * 'a * 'a
      | EAnd of 'a * 'a
      | EOr of 'a * 'a
      | Indexed of 'a * 'a Index.t list
      | Promotion of 'a * UnsizedType.t * UnsizedType.autodifftype
    [@@deriving sexp, hash, compare]

    include Pattern.S with type 'a t := 'a t
  end

  include Fixed.S with module Pattern := Pattern
end

module NoMeta : sig
  module Meta : sig
    type t = unit [@@deriving compare, sexp, hash]

    include Specialized.Meta with type t := unit
  end

  include Specialized.S with module Meta := Meta and type t = Meta.t Fixed.t

  val remove_meta : 'a Fixed.t -> t
end

module Typed : sig
  module Meta : sig
    type t =
      { type_: UnsizedType.t
      ; loc: Location_span.t [@sexp.opaque] [@compare.ignore]
      ; adlevel: UnsizedType.autodifftype }
    [@@deriving compare, create, sexp, hash]

    include Specialized.Meta with type t := t
  end

  include Specialized.S with module Meta := Meta and type t = Meta.t Fixed.t

  val type_of : t -> UnsizedType.t
  val loc_of : t -> Location_span.t
  val adlevel_of : t -> UnsizedType.autodifftype
  val fun_arg : t -> UnsizedType.autodifftype * UnsizedType.t
end

module Labelled : sig
  module Meta : sig
    type t =
      { type_: UnsizedType.t
      ; loc: Location_span.t [@sexp.opaque] [@compare.ignore]
      ; adlevel: UnsizedType.autodifftype
      ; label: Label.Int_label.t }
    [@@deriving compare, create, sexp, hash]

    include Specialized.Meta with type t := t
  end

  include Specialized.S with module Meta := Meta and type t = Meta.t Fixed.t

  val type_of : t -> UnsizedType.t
  val loc_of : t -> Location_span.t
  val adlevel_of : t -> UnsizedType.autodifftype
  val label_of : t -> Label.Int_label.t
  val label : ?init:int -> Typed.t -> t
  val associate : ?init:t Label.Int_label.Map.t -> t -> t Label.Int_label.Map.t

  val associate_index :
    t Label.Int_label.Map.t -> t Index.t -> t Label.Int_label.Map.t
end

module Helpers : sig
  val int : int -> Typed.t
  val float : float -> Typed.t
  val str : string -> Typed.t
  val variable : string -> Typed.t
  val zero : Typed.t
  val one : Typed.t
  val unary_op : Operator.t -> Typed.t -> Typed.t
  val binop : Typed.t -> Operator.t -> Typed.t -> Typed.t
  val binop_list : Typed.t list -> Operator.t -> default:Typed.t -> Typed.t
  val row_vector : float list -> Typed.t
  val vector : float list -> Typed.t
  val matrix : float list list -> Typed.t
  val matrix_from_rows : Typed.t list -> Typed.t
  val array_expr : Typed.t list -> Typed.t
  val try_unpack : Typed.t -> Typed.t list option
  val loop_bottom : Typed.t

  val internal_funapp :
    'a Fixed.t Internal_fun.t -> 'a Fixed.t list -> 'a -> 'a Fixed.t

  val contains_fn_kind :
    ('a Fixed.t Fun_kind.t -> bool) -> ?init:bool -> 'a Fixed.t -> bool

  val infer_type_of_indexed : UnsizedType.t -> 'a Index.t list -> UnsizedType.t
  val add_int_index : Typed.t -> Typed.t Index.t -> Typed.t
  val collect_indices : 'a Fixed.t -> 'a Fixed.t Index.t list
end
