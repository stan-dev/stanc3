(** Abstract syntax. *)
open Core_kernel

(* for auto generating s-exp *)

(** Source code locations *)
type location =
  | Location of Lexing.position * Lexing.position  (** delimited location *)
  | Nowhere  (** no location *)

(** Origin blocks, to keep track of where variables are declared *)
type originblock =
  | MathLibrary
  | Functions
  | Data
  | TData
  | Param
  | TParam
  | Model
  | GQuant

(** Unsized types for function arguments and for decorating expressions
    during type checking; we have a separate type here for Math library
    functions as these functions can be overloaded, so do not have a unique
    type in the usual sense. Still, we want to assign a unique type to every
    expression during type checking.  *)
and unsizedtype =
  | Int
  | Real
  | Vector
  | RowVector
  | Matrix
  | Array of unsizedtype
  | Fun of (originblock * unsizedtype) list * returntype
  | MathLibraryFunction

(** Return types for functions *)
and returntype = Void | ReturnType of unsizedtype

(** Identifiers (variables) *)
and identifier = {name: string; id_loc: location sexp_opaque [@compare.ignore]}

(** Infix operators *)
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

(** Prefix operators *)
and prefixop = Not | UMinus | UPlus

(** Postfix operators *)
and postfixop = Transpose

(** Indices *)
and 'e index =
  | All
  | Single of 'e
  | Upfrom of 'e
  | Downfrom of 'e
  | Between of 'e * 'e
  | Multiple of 'e

(** Expression shapes (used for both typed and untyped expressions, where we
    substitute untyped_expression or typed_expression for 'e *)
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
  (* GetLP is deprecated *)
  | GetLP
  | GetTarget
  | ArrayExpr of 'e list
  | RowVectorExpr of 'e list
  | Paren of 'e
  | Indexed of 'e * 'e index list

(** Meta-data on expressions before type checking: a location for error messages *)
and expression_untyped_metadata =
  {expr_untyped_meta_loc: location sexp_opaque [@compare.ignore]}

(** Meta-data on expressions after type checking: a location, as well as a type
    and an origin block (lub of the origin blocks of the identifiers in it) *)
and expression_typed_metadata =
  { expr_typed_meta_origin_type: originblock * unsizedtype
  ; expr_typed_meta_loc: location sexp_opaque [@compare.ignore] }

(** Untyped expressions *)
and untyped_expression =
  | UntypedExpr of (untyped_expression expression * expression_untyped_metadata)

(** Typed expressions *)
and typed_expression =
  | TypedExpr of (typed_expression expression * expression_typed_metadata)

(** Assignment operators *)
and assignmentoperator =
  | Assign
  (* ArrowAssign is deprecated *)
  | ArrowAssign
  | OperatorAssign of infixop

(** Truncations *)
and 'e truncation =
  | NoTruncate
  | TruncateUpFrom of 'e
  | TruncateDownFrom of 'e
  | TruncateBetween of 'e * 'e

(** Things that can be printed *)
and 'e printable = PString of string | PExpr of 'e

(** Sized types, for variable declarations *)
and 'e sizedtype =
  | SInt
  | SReal
  | SVector of 'e
  | SRowVector of 'e
  | SMatrix of 'e * 'e
  | SArray of 'e sizedtype * 'e

(** Transformations (constraints) for global variable declarations *)
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

(** Statement shapes, where we substitute untyped_expression and untyped_statement
    for 'e and 's respectively to get untyped_statement and typed_expression and
    typed_statement to get typed_statement    *)
and ('e, 's) statement =
  | Assignment of
      { assign_identifier: identifier
      ; assign_indices: 'e index list
      ; assign_op: assignmentoperator
      ; assign_rhs: 'e }
  | NRFunApp of identifier * 'e list
  | TargetPE of 'e
  (* IncrementLogProb is deprecated *)
  | IncrementLogProb of 'e
  | Tilde of
      { arg: 'e
      ; distribution: identifier
      ; args: 'e list
      ; truncation: 'e truncation }
  | Break
  | Continue
  | Return of 'e
  | ReturnVoid
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

(** Meta data for untyped statements: locations for errors *)
and statement_untyped_metadata =
  {stmt_untyped_meta_loc: location sexp_opaque [@compare.ignore]}

(** Statement return types which we will decorate statements with during type
    checking: the purpose is to check that function bodies have the correct
    return type in every possible execution branch.
    NoReturnType corresponds to not having a return statement in it.
    Incomplete rt corresponds to having some return statement(s) of type rt
    in it, but not one in every branch
    Complete rt corresponds to having a return statement of type rt in every branch
    AnyReturnType corresponds to statements which have an error in every branch  *)
and statement_returntype =
  | NoReturnType
  | Incomplete of returntype
  | Complete of returntype
  | AnyReturnType

(** Meta data for typed statements: locations for errors and statement returntypes
    to check that function bodies have the right return type*)
and statement_typed_metadata =
  { stmt_typed_meta_type: statement_returntype
  ; stmt_typed_meta_loc: location sexp_opaque [@compare.ignore] }

(** Untyped statements *)
and untyped_statement =
  | UntypedStmt of
      ( (untyped_expression, untyped_statement) statement
      * statement_untyped_metadata )

(** Typed statements *)
and typed_statement =
  | TypedStmt of
      ((typed_expression, typed_statement) statement * statement_typed_metadata)

(** Program shapes, where we obtain types of programs if we substitute typed or untyped
    statements for 's *)
and 's program =
  { functionblock: 's list option
  ; datablock: 's list option
  ; transformeddatablock: 's list option
  ; parametersblock: 's list option
  ; transformedparametersblock: 's list option
  ; modelblock: 's list option
  ; generatedquantitiesblock: 's list option }

(** Untyped programs (before type checking) *)
and untyped_program = untyped_statement program

(** Typed programs (after type checking) *)
and typed_program = typed_statement program [@@deriving sexp, compare]
