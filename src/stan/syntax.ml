(* Abstract syntax. *)
open Core_kernel

(* for auto generating s-exp *)

(* TODO: add line numbers into metadata for expressions and statements *)

(* == Unsized types == *)
type originblock =
  | Primitives
  | Functions
  | Data
  | TData
  | Param
  | TParam
  | Model
  | GQuant

and unsizedtype =
  | Int
  | Real
  | Vector
  | RowVector
  | Matrix
  | Array of unsizedtype
  | Fun of (originblock * unsizedtype) list * returntype
  | PrimitiveFunction

and returntype = Void | ReturnType of unsizedtype

(* == Expressions == *)
and identifier =
  {name: string; id_loc: Zoo.location sexp_opaque [@compare.ignore]}

and infixop =
  | Plus
  | Minus
  | Times
  | Divide
  | Modulo
  | LDivide
  | EltTimes
  | EltDivide
  | Exp
  | Or
  | And
  | Equals
  | NEquals
  | Less
  | Leq
  | Greater
  | Geq

and prefixop = Not | UMinus | UPlus

and postfixop = Transpose

and 'e index =
  | All
  | Single of 'e
  | Upfrom of 'e
  | Downfrom of 'e
  | Between of 'e * 'e
  | Multiple of 'e

and 'e expression =
  | Conditional of 'e * 'e * 'e
  | InfixOp of 'e * infixop * 'e
  | PrefixOp of prefixop * 'e
  | PostfixOp of 'e * postfixop
  | Variable of identifier
  | IntNumeral of string
  | RealNumeral of string
  | FunApp of identifier * 'e list
  | CondFunApp of identifier * 'e list
  | GetLP
  (* deprecated *)
  | GetTarget
  | ArrayExpr of 'e list
  | RowVectorExpr of 'e list
  | Paren of 'e
  | Indexed of 'e * 'e index list

and expression_untyped_metadata =
  {expr_untyped_meta_loc: Zoo.location sexp_opaque [@compare.ignore]}

and expression_typed_metadata =
  { expr_typed_meta_origin_type: originblock * unsizedtype
  ; expr_typed_meta_loc: Zoo.location sexp_opaque [@compare.ignore] }

and untyped_expression =
  | UntypedExpr of (untyped_expression expression * expression_untyped_metadata)

and typed_expression =
  | TypedExpr of (typed_expression expression * expression_typed_metadata)

(* == Statements == *)
and assignmentoperator =
  | Assign
  | PlusAssign
  | MinusAssign
  | TimesAssign
  | DivideAssign
  | EltTimesAssign
  | EltDivideAssign
  | ArrowAssign

(* deprecated *)
and 'e truncation =
  | NoTruncate
  | TruncateUpFrom of 'e
  | TruncateDownFrom of 'e
  | TruncateBetween of 'e * 'e

and 'e printable = PString of string | PExpr of 'e

and 'e sizedtype =
  | SInt
  | SReal
  | SVector of 'e
  | SRowVector of 'e
  | SMatrix of 'e * 'e
  | SArray of 'e sizedtype * 'e

and 'e transformation =
  | Identity
  | Lower of 'e
  | Upper of 'e
  | LowerUpper of 'e * 'e
  | LocationScale of 'e * 'e
  | Ordered
  | PositiveOrdered
  | Simplex
  | UnitVector
  | CholeskyCorr
  | CholeskyCov
  | Correlation
  | Covariance

and ('e, 's) statement =
  | Assignment of
      { assign_identifier: identifier
      ; assign_indices: 'e index list
      ; assign_op: assignmentoperator
      ; assign_rhs: 'e }
  | NRFunApp of identifier * 'e list
  | TargetPE of 'e
  | IncrementLogProb of 'e
  (* deprecated *)
  | Tilde of
      { arg: 'e
      ; distribution: identifier
      ; args: 'e list
      ; truncation: 'e truncation }
  | Break
  | Continue
  | Return of 'e
  | Print of 'e printable list
  | Reject of 'e printable list
  | Skip
  | IfThenElse of 'e * 's * 's
  | IfThen of 'e * 's
  | While of 'e * 's
  | For of
      { loop_variable: identifier
      ; lower_bound: 'e
      ; upper_bound: 'e
      ; loop_body: 's }
  | ForEach of identifier * 'e * 's
  | Block of 's list
  | VDecl of 'e sizedtype * identifier
  | VDeclAss of {sizedtype: 'e sizedtype; identifier: identifier; value: 'e}
  | TVDecl of 'e sizedtype * 'e transformation * identifier
  | TVDeclAss of
      { tsizedtype: 'e sizedtype
      ; transformation: 'e transformation
      ; tidentifier: identifier
      ; tvalue: 'e }
  | FunDef of
      { returntype: returntype
      ; funname: identifier
      ; arguments: (originblock * unsizedtype * identifier) list
      ; body: 's }

and statement_untyped_metadata =
  {stmt_untyped_meta_loc: Zoo.location sexp_opaque [@compare.ignore]}

and statement_typed_metadata =
  { stmt_typed_meta_type: returntype
  ; stmt_typed_meta_loc: Zoo.location sexp_opaque [@compare.ignore] }

and untyped_statement =
  | UntypedStmt of
      ( (untyped_expression, untyped_statement) statement
      * statement_untyped_metadata )

and typed_statement =
  | TypedStmt of
      ((typed_expression, typed_statement) statement * statement_typed_metadata)

(* == Programs == *)
and 's program =
  { functionblock: 's list option
  ; datablock: 's list option
  ; transformeddatablock: 's list option
  ; parametersblock: 's list option
  ; transformedparametersblock: 's list option
  ; modelblock: 's list option
  ; generatedquantitiesblock: 's list option }

and untyped_program = untyped_statement program

and typed_program = typed_statement program [@@deriving sexp, compare]

(* == Stuff that probably should be moved to another file == *)
type signaturestype = returntype * returntype list [@@deriving sexp, compare]

(* TODO: maybe move these to primitives file, as that's where they're used *)

let string_of_expressiontype = function
  | _, ut -> Sexp.to_string (sexp_of_unsizedtype ut)

let string_of_returntype = function
  | rt -> Sexp.to_string (sexp_of_returntype rt)

let string_of_opt_expressiontype = function
  | None -> "unknown"
  | Some x -> string_of_expressiontype x

(* TODO: implement more pretty printing functions for generating error messages *)
