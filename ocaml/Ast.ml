type unaryOp = Not | Neg
type binOp = Add | Sub | Mul | Div | Mod | CoeffMul
           | And | Or | Lte | Gte | Neq | Eq | Lt | Gt

type ident = string

type expr = Var of ident
          | IntLit of int
          | NumLit of float
          | StrLit of string
          | UnaryOp of unaryOp * expr
          | BinOp of binOp * expr * expr
          | FCall of ident * expr list
          | If of expr * expr * expr

type typePrim = TBool | TReal | TInt
              | TArray of typePrim * expr list
              | TVector of expr
              | TMatrix of expr * expr
              | TUnit

type typeLevel = LevelVar of string
               | Data | Model | GenQuant
               | Lub of typeLevel list | Glb of typeLevel list

type fullType = typePrim * typeLevel

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

type stanProg = Prog of funDef list * statement * statement
              | None

let next() = "0"
