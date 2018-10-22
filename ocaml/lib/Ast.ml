open Core_kernel

type expr = Var of string
          | IntLit of int
          | NumLit of float
          | StrLit of string
          | FnApp of string * expr list
          | If of expr * expr * expr
          | AssignExpr of string * expr
          | ExprList of expr list
[@@deriving sexp, hash]

type typePrim = TBool | TReal | TInt
              | TArray of typePrim * expr list
              | TVector of expr
              | TMatrix of expr * expr
              | TUnit

type typeLevel = LevelVar of string
               | Data | Model | GenQuant
               | Lub of typeLevel list | Glb of typeLevel list

type fullType = typePrim

type binOp = Add | Sub | Mul | Div | Mod | CoeffMul
           | And | Or | Lte | Gte | Neq | Eq | Lt | Gt

type statement = Block of statement list
               | DistAs of string * expr list
               | Assign of binOp * string * expr
               | VarDecl of fullType * string
               | FCall of string * expr list
               | If of expr * statement * statement
               | While of expr * statement
               | For of string * expr * expr * statement
               | Break | Continue | Unit

type arg = fullType * string

type funDef = FunExpr of string * arg list * statement * expr
            | FunVoid of string * arg list * statement * unit

(*
type stanProg = Prog of funDef list * statement * statement
              | None
*)

type stanProg = Prog of expr
              | None
[@@deriving sexp]

let next() = "0"
