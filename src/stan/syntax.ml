(* Abstract syntax. *)
open Core_kernel

(* for auto generating s-exp *)

(* TODO: add line numbers into metadata for expressions and statements *)

(* == Unsized types == *)
(* TODO: add primitive origin here, so everything has an origin *)
type originblock =
  | Primitives
  | Functions
  | Data
  | TData
  | Param
  | TParam
  | Model
  | GQuant

(* TODO: Add primitive function type here, for type checking higher order functions later.
   That way, every expression can have an unsizedtype *)
and unsizedtype =
  | Int
  | Real
  | Vector
  | RowVector
  | Matrix
  | Array of unsizedtype
  | Fun of (originblock * unsizedtype) list * returntype
  | PrimitiveFunction

(* TODO: set up semantic check such that types are always defined. *)
and returntype = Void | ReturnType of unsizedtype

(* == Expressions == *)
and identifier = string

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

and 'em index =
  | All
  | Single of 'em
  | Upfrom of 'em
  | Downfrom of 'em
  | Between of 'em * 'em
  | Multiple of 'em

and 'em expression =
  | Conditional of 'em * 'em * 'em
  | InfixOp of 'em * infixop * 'em
  | PrefixOp of prefixop * 'em
  | PostfixOp of 'em * postfixop
  | Variable of identifier
  | IntNumeral of string
  | RealNumeral of string
  | FunApp of identifier * 'em list
  | CondFunApp of identifier * 'em list
  | GetLP
  (* deprecated *)
  | GetTarget
  | ArrayExpr of 'em list
  | RowVectorExpr of 'em list
  | Paren of 'em
  | Indexed of 'em * 'em index list

and expression_untyped_metadata =
  {expr_meta_none: (originblock * unsizedtype) option}

and expression_typed_metadata =
  {expr_meta_origintype: (originblock * unsizedtype) option}

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
and 'em truncation =
  | NoTruncate
  | TruncateUpFrom of 'em
  | TruncateDownFrom of 'em
  | TruncateBetween of 'em * 'em

and 'em printable = PString of string | PExpr of 'em

and 'em sizedtype =
  | SInt
  | SReal
  | SVector of 'em
  | SRowVector of 'em
  | SMatrix of 'em * 'em
  | SArray of 'em sizedtype * 'em

and 'em transformation =
  | Identity
  | Lower of 'em
  | Upper of 'em
  | LowerUpper of 'em * 'em
  | LocationScale of 'em * 'em
  | Ordered
  | PositiveOrdered
  | Simplex
  | UnitVector
  | CholeskyCorr
  | CholeskyCov
  | Correlation
  | Covariance

and ('em, 'sm) statement =
  | Assignment of
      { assign_identifier: identifier
      ; assign_indices: 'em index list
      ; assign_op: assignmentoperator
      ; assign_rhs: 'em }
  | NRFunApp of identifier * 'em list
  | TargetPE of 'em
  | IncrementLogProb of 'em
  (* deprecated *)
  | Tilde of
      { arg: 'em
      ; distribution: identifier
      ; args: 'em list
      ; truncation: 'em truncation }
  | Break
  | Continue
  | Return of 'em
  | Print of 'em printable list
  | Reject of 'em printable list
  | Skip
  | IfThenElse of 'em * 'sm * 'sm
  | IfThen of 'em * 'sm
  | While of 'em * 'sm
  | For of
      { loop_variable: identifier
      ; lower_bound: 'em
      ; upper_bound: 'em
      ; loop_body: 'sm }
  | ForEach of identifier * 'em * 'sm
  | Block of 'sm list
  | VDecl of 'em sizedtype * identifier
  | VDeclAss of {sizedtype: 'em sizedtype; identifier: identifier; value: 'em}
  | TVDecl of 'em sizedtype * 'em transformation * identifier
  | TVDeclAss of
      { tsizedtype: 'em sizedtype
      ; transformation: 'em transformation
      ; tidentifier: identifier
      ; tvalue: 'em }
  | FunDef of
      { returntype: returntype
      ; name: identifier
      ; arguments: (originblock * unsizedtype * identifier) list
      ; body: 'sm }

and statement_untyped_metadata = {stmt_meta_none: returntype option}

and statement_typed_metadata = {stmt_meta_type: returntype option}

and untyped_statement =
  | UntypedStmt of
      ( (untyped_expression, untyped_statement) statement
      * statement_untyped_metadata )

and typed_statement =
  | TypedStmt of
      ((typed_expression, typed_statement) statement * statement_typed_metadata)

(* == Programs == *)
and 'sm program =
  { functionblock: 'sm list option
  ; datablock: 'sm list option
  ; transformeddatablock: 'sm list option
  ; parametersblock: 'sm list option
  ; transformedparametersblock: 'sm list option
  ; modelblock: 'sm list option
  ; generatedquantitiesblock: 'sm list option }

and untyped_program = untyped_statement program

and typed_program = typed_statement program [@@deriving sexp, compare]

(* == Stuff that probably should be moved to another file == *)
type signaturestype = returntype * returntype list [@@deriving sexp, compare]

(* TODO: maybe move these to primitives file, as that's where they're used *)

let string_of_expressiontype = function
  | None -> "unknown"
  | Some (_, ut) -> Sexp.to_string (sexp_of_unsizedtype ut)

(* TODO: implement more pretty printing functions for generating error messages *)

(* TODO: get rid of the Some's and None's *)
