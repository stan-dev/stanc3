open Core_kernel

type ident = string
[@@deriving sexp, hash]

type expr = Var of ident
          | IntLit of int
          | NumLit of float
          | StrLit of string
          | FnApp of ident * expr list
          | If of expr * expr * expr
          | AssignExpr of string * expr
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
               | DistAs of ident * expr list
               | Assign of binOp * ident * expr
               | VarDecl of fullType * ident
               | FCall of ident * expr list
               | If of expr * statement * statement
               | While of expr * statement
               | For of ident * expr * expr * statement
               | Break | Continue | Unit

type arg = fullType * ident

type funDef = FunExpr of ident * arg list * statement * expr
            | FunVoid of ident * arg list * statement * unit

(*
type stanProg = Prog of funDef list * statement * statement
              | None
*)

type stanProg = Prog of expr
              | None
[@@deriving sexp]

let next() = "0"
