open Common

type t = Mir_pattern.unsizedtype =
  | UInt
  | UReal
  | UVector
  | URowVector
  | UMatrix
  | UArray of t
  | UFun of (autodifftype * t) list * returntype
  | UMathLibraryFunction

and autodifftype = Mir_pattern.autodifftype = DataOnly | AutoDiffable

and returntype = Mir_pattern.returntype = Void | ReturnType of t
[@@deriving compare, hash, sexp]

include Pretty.S with type t := t

val pp_returntype : Format.formatter -> returntype -> unit
val pp_autodifftype : Format.formatter -> autodifftype -> unit
val uint : t
val ureal : t
val uvector : t
val urowvector : t
val umatrix : t
val uarray : t -> t
val ufun : (autodifftype * t) list -> t -> t
val ufun_void : (autodifftype * t) list -> t
val umathlibfun : t
val autodifftype_can_convert : autodifftype -> autodifftype -> bool
val check_of_same_type_mod_conv : string -> t -> t -> bool
val check_of_same_type_mod_array_conv : string -> t -> t -> bool

val check_compatible_arguments_mod_conv :
  string -> (autodifftype * t) list -> (autodifftype * t) list -> bool

val is_real_type : t -> bool
val is_int_type : t -> bool
val is_fun_type  : t -> bool
