(* Abstract syntax. *)
open Core_kernel

(* for auto generating s-exp *)

(* TODO: add line numbers into metadata for expressions and statements *)

(* == Unsized types == *)
(* TODO: add primitive origin here, so everything has an origin *)
type originblock = Functions | Data | TData | Param | TParam | Model | GQuant

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

and index =
  | All
  | Single of expression
  | Upfrom of expression
  | Downfrom of expression
  | Between of expression * expression
  | Multiple of expression

and untypedexpression =
  | Conditional of expression * expression * expression
  | InfixOp of expression * infixop * expression
  | PrefixOp of prefixop * expression
  | PostfixOp of expression * postfixop
  | Variable of identifier
  | IntNumeral of string
  | RealNumeral of string
  | FunApp of identifier * expression list
  | CondFunApp of identifier * expression list
  | GetLP
  (* deprecated *)
  | GetTarget
  | ArrayExpr of expression list
  | RowVectorExpr of expression list
  | Paren of expression
  | Indexed of expression * index list

and expression_metadata =
  {expr_meta_origintype: (originblock * unsizedtype) option}

and expression = untypedexpression * expression_metadata

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
and truncation =
  | NoTruncate
  | TruncateUpFrom of expression
  | TruncateDownFrom of expression
  | TruncateBetween of expression * expression

and printable = PString of string | PExpr of expression

and sizedtype =
  | SInt
  | SReal
  | SVector of expression
  | SRowVector of expression
  | SMatrix of expression * expression
  | SArray of sizedtype * expression

and transformation =
  | Identity
  | Lower of expression
  | Upper of expression
  | LowerUpper of expression * expression
  | LocationScale of expression * expression
  | Ordered
  | PositiveOrdered
  | Simplex
  | UnitVector
  | CholeskyCorr
  | CholeskyCov
  | Correlation
  | Covariance

and untypedstatement =
  | Assignment of
      { assign_identifier: identifier
      ; assign_indices: index list
      ; assign_op: assignmentoperator
      ; assign_rhs: expression }
  | NRFunApp of identifier * expression list
  | TargetPE of expression
  | IncrementLogProb of expression
  (* deprecated *)
  | Tilde of
      { arg: expression
      ; distribution: identifier
      ; args: expression list
      ; truncation: truncation }
  | Break
  | Continue
  | Return of expression
  | Print of printable list
  | Reject of printable list
  | Skip
  | IfThenElse of expression * statement * statement
  | IfThen of expression * statement
  | While of expression * statement
  | For of
      { loop_variable: identifier
      ; lower_bound: expression
      ; upper_bound: expression
      ; loop_body: statement }
  | ForEach of identifier * expression * statement
  | Block of statement list
  | VDecl of sizedtype * identifier
  | VDeclAss of
      { sizedtype: sizedtype
      ; identifier: identifier
      ; value: expression }
  | TVDecl of sizedtype * transformation * identifier
  | TVDeclAss of
      { tsizedtype: sizedtype
      ; transformation: transformation
      ; tidentifier: identifier
      ; tvalue: expression }
  | FunDef of
      { returntype: returntype
      ; name: identifier
      ; arguments: (originblock * unsizedtype * identifier) list
      ; body: statement }

and statement_metadata = {stmt_meta_type: returntype option}

(* TODO: add vardecl/topvardecl/fundef/compound vardecl/compound topvardecl here?
   then we only need to add metadata to statements and expressions *)
and statement = untypedstatement * statement_metadata

(* TODO: Decorate fundef with optional marker like RNG, LP, PLAIN *)

(* == Programs == *)
and program =
  { functionblock: statement list option
  ; datablock: statement list option
  ; transformeddatablock: statement list option
  ; parametersblock: statement list option
  ; transformedparametersblock: statement list option
  ; modelblock: statement list option
  ; generatedquantitiesblock: statement list option }
[@@deriving sexp, compare]

(* == Stuff that probably should be moved to another file == *)
type signaturestype = returntype * returntype list [@@deriving sexp, compare]

(* TODO: maybe move these to primitives file, as that's where they're used *)

let string_of_expressiontype = function
  | None -> "unknown"
  | Some (_, ut) -> Sexp.to_string (sexp_of_unsizedtype ut)

(* TODO: implement more pretty printing functions for generating error messages *)
