open Core_kernel

module type Mir = sig
  (** The Middle Intermediate Representation, which program transformations
    operate on *)

  (** Source code locations *)
  type location =
    { filename: string
    ; line_num: int
    ; col_num: int
    ; included_from: location option }
  [@@deriving sexp]

  (** Delimited locations *)
  type location_span = {begin_loc: location; end_loc: location}
  [@@deriving sexp]

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
    | And
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
  [@@deriving sexp, compare, map, hash]

  val remove_size : 'a sizedtype -> unsizedtype

  (** remove_size [st] discards size information from a sizedtype
    to return an unsizedtype. *)

  type litType = Int | Real | Str [@@deriving sexp, hash]

  (**  *)
  type fun_kind = StanLib | CompilerInternal | UserDefined
  [@@deriving compare, sexp, hash]

  type 'e index =
    | All
    | Single of 'e
    (*
  | MatrixSingle of 'e
 *)
    | Upfrom of 'e
    | Downfrom of 'e
    | Between of 'e * 'e
    | MultiIndex of 'e
  [@@deriving sexp, hash, map]

  and 'e expr =
    | Var of string
    | Lit of litType * string
    | FunApp of fun_kind * string * 'e list
    | TernaryIf of 'e * 'e * 'e
    | EAnd of 'e * 'e
    | EOr of 'e * 'e
    | Indexed of 'e * 'e index list
  [@@deriving sexp, hash, map]

  type fun_arg_decl = (autodifftype * string * unsizedtype) list
  [@@deriving sexp, hash, map]

  type 's fun_def =
    { fdrt: unsizedtype option
    ; fdname: string
    ; fdargs: fun_arg_decl
    ; fdbody: 's
    ; fdloc: location_span [@compare.ignore] }
  [@@deriving sexp, hash, map]

  and 'e lvalue = string * 'e index list

  and ('e, 's) statement =
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
        ; decl_type: 'e sizedtype }
  [@@deriving sexp, hash, map]

  type io_block =
    | Data
    | Parameters
    | TransformedParameters
    | GeneratedQuantities
  [@@deriving sexp, hash]

  type 'e io_var = string * ('e sizedtype * io_block) [@@deriving sexp]

  type ('e, 's) prog =
    { functions_block: 's fun_def list
    ; input_vars: 'e io_var list
    ; prepare_data: 's list (* data & transformed data decls and statements *)
    ; log_prob: 's list (*assumes data & params are in scope and ready*)
    ; generate_quantities: 's list (* assumes data & params ready & in scope*)
    ; transform_inits: 's list
    ; output_vars: 'e io_var list
    ; prog_name: string
    ; prog_path: string }
  [@@deriving sexp]

  type 'm with_expr = {expr: 'm with_expr expr; emeta: 'm}

  and mtype_loc_ad =
    { mtype: unsizedtype
    ; mloc: location_span sexp_opaque [@compare.ignore]
    ; madlevel: autodifftype }
  [@@deriving sexp]

  type ('e, 'm) stmt_with =
    {stmt: ('e with_expr, ('e, 'm) stmt_with) statement; smeta: 'm}

  and stmt_loc =
    (mtype_loc_ad, (location_span sexp_opaque[@compare.ignore])) stmt_with
  [@@deriving sexp]

  type expr_no_meta = unit with_expr
  type stmt_no_meta = (expr_no_meta, unit) stmt_with
  type typed_prog = (mtype_loc_ad with_expr, stmt_loc) prog [@@deriving sexp]

  type internal_fn =
    | FnLength
    | FnMakeArray
    | FnMakeRowVec
    | FnNegInf
    | FnReadData
    | FnReadParam
    | FnWriteParam
    | FnConstrain
    | FnUnconstrain
    | FnCheck
    | FnPrint
    | FnReject
  [@@deriving sexp]
end
