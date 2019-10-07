(** The Middle Intermediate Representation, which program transformations
    operate on *)

open Core_kernel

(** Source code locations *)
type location =
  { filename: string
  ; line_num: int
  ; col_num: int
  ; included_from: location option }
[@@deriving sexp, hash, compare]

(** Delimited locations *)
type location_span = {begin_loc: location; end_loc: location}
[@@deriving sexp, hash, compare]

(** Arithmetic and logical operators *)
type operator =
  | Plus
  | PPlus
  | Minus
  | PMinus
  | Times
  | Divide
  | Modulo
  | LDivide
  | EltTimes
  | EltDivide
  | Pow
  | Or
  (* deprecated?*)
  | And
  (* deprecated?*)
  | Equals
  | NEquals
  | Less
  | Leq
  | Greater
  | Geq
  | PNot
  | Transpose
[@@deriving sexp, hash, compare]

(** Unsized types for function arguments and for decorating expressions
    during type checking; we have a separate type here for Math library
    functions as these functions can be overloaded, so do not have a unique
    type in the usual sense. Still, we want to assign a unique type to every
    expression during type checking.  *)
type unsizedtype =
  | UInt
  | UReal
  | UVector
  | URowVector
  | UMatrix
  | UArray of unsizedtype
  | UFun of (autodifftype * unsizedtype) list * returntype
  | UMathLibraryFunction
[@@deriving sexp, hash]

(** Flags for data only arguments to functions *)
and autodifftype = DataOnly | AutoDiffable [@@deriving sexp, hash, compare]

and returntype = Void | ReturnType of unsizedtype [@@deriving sexp, hash]

(** Sized types, for variable declarations *)
type 'e sizedtype =
  | SInt
  | SReal
  | SVector of 'e
  | SRowVector of 'e
  | SMatrix of 'e * 'e
  | SArray of 'e sizedtype * 'e
[@@deriving sexp, compare, map, hash, fold]

type 'e possiblysizedtype = Sized of 'e sizedtype | Unsized of unsizedtype
[@@deriving sexp, compare, map, hash, fold]

type litType = Int | Real | Str [@@deriving sexp, hash, compare]

(**  *)
type fun_kind = StanLib | CompilerInternal | UserDefined
[@@deriving compare, sexp, hash]

type 'e index =
  | All
  | Single of 'e
  | Upfrom of 'e
  | Between of 'e * 'e
  | MultiIndex of 'e
[@@deriving sexp, hash, map, fold]

and 'e expr =
  | Var of string
  | Lit of litType * string
  | FunApp of fun_kind * string * 'e list
  | TernaryIf of 'e * 'e * 'e
  | EAnd of 'e * 'e
  | EOr of 'e * 'e
  | Indexed of 'e * 'e index list
[@@deriving sexp, hash, map, compare, fold]

type fun_arg_decl = (autodifftype * string * unsizedtype) list
[@@deriving sexp, hash, map]

type 's fun_def =
  { fdrt: unsizedtype option
  ; fdname: string
  ; fdargs: fun_arg_decl
  ; fdbody: 's
  ; fdloc: location_span sexp_opaque [@compare.ignore] }
[@@deriving sexp, hash, map]

type 'e lvalue = string * unsizedtype * 'e index list
[@@deriving sexp, hash, map, fold]

type ('e, 's) statement =
  | Assignment of 'e lvalue * 'e
  | TargetPE of 'e
  | NRFunApp of fun_kind * string * 'e list
  | Break
  | Continue
  | Return of 'e option
  | Skip
  | IfElse of 'e * 's * 's option
  | While of 'e * 's
  (* XXX Collapse with For? *)
  | For of {loopvar: string; lower: 'e; upper: 'e; body: 's}
  (* A Block for now corresponds tightly with a C++ block:
     variables declared within it have local scope and are garbage collected
     when the block ends.*)
  | Block of 's list
  (* SList has no semantics, just programming convenience *)
  | SList of 's list
  | Decl of
      { decl_adtype: autodifftype
      ; decl_id: string
      ; decl_type: 'e possiblysizedtype }
[@@deriving sexp, hash, map, fold]

type io_block = Parameters | TransformedParameters | GeneratedQuantities
[@@deriving sexp, hash]

(** Transformations (constraints) for global variable declarations *)
type 'e transformation =
  | Identity
  | Lower of 'e
  | Upper of 'e
  | LowerUpper of 'e * 'e
  | Offset of 'e
  | Multiplier of 'e
  | OffsetMultiplier of 'e * 'e
  | Ordered
  | PositiveOrdered
  | Simplex
  | UnitVector
  | CholeskyCorr
  | CholeskyCov
  | Correlation
  | Covariance
[@@deriving sexp, compare, map, hash]

type 'e outvar =
  { out_unconstrained_st: 'e sizedtype
  ; out_constrained_st: 'e sizedtype
  ; out_block: io_block
  ; out_trans: 'e transformation }
[@@deriving sexp, map, hash]

type ('e, 's) prog =
  { functions_block: 's fun_def list
  ; input_vars: (string * 'e sizedtype) list (* AS READ IN, ie unconstrained *)
  ; prepare_data: 's list (* data & transformed data decls and statements *)
  ; log_prob: 's list (*assumes data & params are in scope and ready*)
  ; generate_quantities: 's list (* assumes data & params ready & in scope*)
  ; transform_inits: 's list
  ; output_vars: (string * 'e outvar) list
  ; prog_name: string
  ; prog_path: string }
[@@deriving sexp, map]

type 'm with_expr = {expr: 'm with_expr expr; emeta: 'm}
[@@deriving compare, sexp, hash]

type mtype_loc_ad =
  { mtype: unsizedtype
  ; mloc: location_span sexp_opaque [@compare.ignore]
  ; madlevel: autodifftype }
[@@deriving compare, sexp, hash]

type ('e, 'm) stmt_with =
  {stmt: ('e with_expr, ('e, 'm) stmt_with) statement; smeta: 'm}
[@@deriving sexp]

type ('e, 'm) stmt_with_num = {stmtn: ('e with_expr, int) statement; smetan: 'm}
[@@deriving sexp, hash]

type expr_no_meta = unit with_expr [@@deriving sexp]

type expr_typed_located = mtype_loc_ad with_expr
[@@deriving sexp, compare, hash]

type stmt_no_meta = (expr_no_meta, unit) stmt_with [@@deriving sexp]

type stmt_loc =
  (mtype_loc_ad, (location_span sexp_opaque[@compare.ignore])) stmt_with
[@@deriving sexp]

type stmt_loc_num =
  (mtype_loc_ad, (location_span sexp_opaque[@compare.ignore])) stmt_with_num
[@@deriving sexp]

type typed_prog = (mtype_loc_ad with_expr, stmt_loc) prog [@@deriving sexp]

type internal_fn =
  | FnLength
  | FnMakeArray
  | FnMakeRowVec
  | FnNegInf
  | FnReadData
  (* XXX move these to a backend specific file?*)
  | FnReadParam
  | FnWriteParam
  | FnConstrain
  | FnUnconstrain
  | FnCheck
  | FnPrint
  | FnReject
  | FnResizeToMatch
  | FnNaN
[@@deriving sexp]

type flag_vars = EmitGeneratedQuantities | EmitTransformedParameters

let all_flag_vars = [EmitGeneratedQuantities; EmitTransformedParameters]

let string_of_flag_var (flag_var : flag_vars) : string =
  match flag_var with
  | EmitGeneratedQuantities -> "emit_generated_quantities__"
  | EmitTransformedParameters -> "emit_transformed_parameters__"

(**  A custom comparator which ignores locations on expressions *)
module ExprComparator = struct
  type t = expr_typed_located [@@deriving sexp, compare]
end

(**  A module for sets of expressions which ignore their locations *)
module ExprSet = Set.Make (ExprComparator)

(**  A module for maps of expressions which ignore their locations *)
module ExprMap = Map.Make (ExprComparator)
