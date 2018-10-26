(* Abstract syntax. *)
open Core_kernel (* for auto generating s-exp *)

(* TODO: constructors to take multiple arguments, rather than a tuple? *)

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

and

(* Blocks. *)
functionblock =
  | EmptyFunBlock
  | FunBlock of (fundef list)

and

datablock =
  | EmptyDataBlock
  | DataBlock of (topvardecl list)

and

transformeddatablock =
  | EmptyTDataBlock
  | TDataBlock of (topvardecl_or_statement list)

and

parametersblock =
  | EmptyParamBlock
  | ParamBlock of (topvardecl list)

and

transformedparametersblock =
  | EmptyTParamBlock
  | TParamBlock of (topvardecl_or_statement list)

and

modelblock =
  | EmptyModelBlock
  | ModelBlock of (vardecl_or_statement list)

and

generatedquantitiesblock =
  | EmptyGQBlock
  | GQBlock of (topvardecl_or_statement list)

and

(* Declarations and definitions *)
fundef =
  | FunDef of returntype * identifier * (argdecl list) * statement

and

identifier = string

and

real = float

and

size = int64

and

argdecl =
  | DataArg of unsizedtype * identifier
  | Arg of unsizedtype * identifier

and

returntype =
  | Void
  | ReturnType of unsizedtype

and

unsizedtype = basictype * size (* int is for the array dimensions *)

and

basictype =
  | Int
  | Real
  | Vector
  | RowVector
  | Matrix

and

sizedbasictype =
  | SInt
  | SReal
  | SVector of expression
  | SRowVector of expression
  | SMatrix of expression * expression

and

topvarbasictype = sizedbasictype * transformation

and

topvardecl = topvartype * identifier

and

vardecl = sizedtype * identifier

and

topvardecl_or_statement =
  | TVDecl of topvardecl
  | TStmt of statement

and

vardecl_or_statement =
  | VDecl of vardecl
  | Stmt of statement

and

topvartype =  topvarbasictype * (expression list) (* list of expression for the array dimension sizes *)

and

sizedtype =  sizedbasictype * (expression list) (* list of expression for the array dimension sizes *)

and

transformation =
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

and

(* Expressions. *)
expression =
  | Conditional of expression * expression * expression
  | InfixOp of expression * infixop * expression
  | PrefixOp of prefixop * expression
  | PostfixOp of expression * postfixop
  | Variable of identifier (* a variable *)
  | IntNumeral of int64 (* integer constant *)
  | RealNumeral of real (* real constant *)
  | FunApp of identifier * (expression list)
  | CondFunApp of identifier * (expression list)
  | GetLP (* deprecated *)
  | GetTarget
  | ArrayExpr of (expression list)
  | RowVectorExpr of (expression list)
  | Paren of expression
  | Indexed of expression * (index list)

and

infixop =
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

and

prefixop =
  | Not
  | UMinus
  | UPlus

and

postfixop =
  | Transpose

and

printable =
  | PString of string
  | PExpr of expression

and

(* Statements. *)
statement =
  | Assignment of lhs * assignmentoperator * expression
  | NRFunApp of identifier * (expression list)
  | TargetPE of expression
  | IncrementLogProb of expression (* deprecated *)
  | Tilde of expression * identifier * (expression list) * truncation
  | Break
  | Continue
  | Return of expression
  | Print of (printable list)
  | Reject of (printable list)
  | Skip
  | IfElse of expression * statement * statement
  | While of expression * statement
  | For of identifier * expression * expression * statement
  | ForEach of identifier * expression * statement
  | Block of (vardecl_or_statement list)

and

truncation =
  | NoTruncate
  | TruncateUpFrom of expression
  | TruncateDownFrom of expression
  | TruncateBetween of expression * expression

and

lhs = identifier * (index  list)

and

index =
  | All
  | Single of expression
  | Upfrom of expression
  | Downfrom of expression
  | Between of expression * expression

and

assignmentoperator =
  | Assign
  | PlusAssign
  | MinusAssign
  | TimesAssign
  | DivideAssign
  | EltTimesAssign
  | EltDivideAssign
  | ArrowAssign (* deprecated *)


[@@deriving sexp]
