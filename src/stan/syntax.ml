(* Abstract syntax. *)
open Core_kernel

(* for auto generating s-exp *)

(* TODO: Reorder and get rid of some 'and's *)

(* TODO: Get rid of some redundant intermediate definitions. *)

(* TODO: Change some tuples into records *)

(* TODO: Change some EmptyFunBlocks into list option etc. *)

(* Programs. *)
type program =
  | Program of
      functionblock
      * datablock
      * transformeddatablock
      * parametersblock
      * transformedparametersblock
      * modelblock
      * generatedquantitiesblock

(* Blocks. *)
and functionblock = fundef list option

and datablock = topvardecl list option

and transformeddatablock = topvardecl_or_statement list option

and parametersblock = topvardecl list option

and transformedparametersblock = topvardecl_or_statement list option

and modelblock = vardecl_or_statement list option

and generatedquantitiesblock = topvardecl_or_statement list option

(* Declarations and definitions *)
and fundef = FunDef of returntype * identifier * argdecl list * statement

and identifier = string

(* TODO: represent numerals as strings *)
and size = int64

and argdecl =
  | DataArg of unsizedtype * identifier
  | Arg of unsizedtype * identifier

and returntype = Void | ReturnType of unsizedtype

and unsizedtype =
  | Int
  | Real
  | Vector
  | RowVector
  | Matrix
  | Array of unsizedtype

and topvardecl = topvartype * identifier

and vardecl = sizedtype * identifier

and topvardecl_or_statement = TVDecl of topvardecl | TStmt of statement

and vardecl_or_statement = VDecl of vardecl | Stmt of statement

and topvartype = sizedtype * transformation

and sizedtype =
  | SInt
  | SReal
  | SVector of expression
  | SRowVector of expression
  | SMatrix of expression * expression
  | SArray of sizedtype * expression

(* expression is for size *)
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

(* Expressions. *)
and expression =
  | Conditional of expression * expression * expression
  | InfixOp of expression * infixop * expression
  | PrefixOp of prefixop * expression
  | PostfixOp of expression * postfixop
  | Variable of identifier
  (* a variable *)
  | IntNumeral of string
  (* integer constant *)
  | RealNumeral of string
  (* real constant *)
  | FunApp of identifier * expression list
  | CondFunApp of identifier * expression list
  | GetLP
  (* deprecated *)
  | GetTarget
  | ArrayExpr of expression list
  | RowVectorExpr of expression list
  | Paren of expression
  | Indexed of expression * index list

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

and printable = PString of string | PExpr of expression

(* Statements. *)
and statement =
  | Assignment of lhs * assignmentoperator * expression
  | NRFunApp of identifier * expression list
  | TargetPE of expression
  | IncrementLogProb of expression
  (* deprecated *)
  | Tilde of expression * identifier * expression list * truncation
  | Break
  | Continue
  | Return of expression
  | Print of printable list
  | Reject of printable list
  | Skip
  | IfElse of expression * statement * statement
  | While of expression * statement
  | For of identifier * expression * expression * statement
  | ForEach of identifier * expression * statement
  | Block of vardecl_or_statement list

and truncation =
  | NoTruncate
  | TruncateUpFrom of expression
  | TruncateDownFrom of expression
  | TruncateBetween of expression * expression

and lhs = identifier * index list

and index =
  | All
  | Single of expression
  | Upfrom of expression
  | Downfrom of expression
  | Between of expression * expression

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
[@@deriving sexp]
