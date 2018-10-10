//XXX Add line numbers...?

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Expr {
    Int(i32),
    Str(String),
    Real(String),
    Var(String),
    FnApp(String, Vec<Box<Expr>>),
}

pub enum TypePrim {
    Bool,
    Real,
    Int,
    Array(Box<TypePrim>, Vec<Expr>),
    Vector(Box<Expr>),
    Matrix(Box<Expr>, Box<Expr>),
    Unit,
}

pub enum TypeLevel {
    LevelVar(String),
    Data,
    Model,
    GenQuant,
    Lub(Vec<TypeLevel>),
    Glb(Vec<TypeLevel>),
}

// FullType = (TypePrim, TypeLevel)

// type statement = Block of statement list
//     | DistAs of ident * expr list
//     | Assign of binOp * ident * expr
//     | VarDecl of fullType * ident
//     | FCall of ident * expr list
//     | If of expr * statement * statement
//     | While of expr * statement
//     | For of ident * expr * expr * statement
//     | Break | Continue | Unit
//
// type arg = fullType * ident
//
// type funDef = FunExpr of ident * arg list * statement * expr
//     | FunVoid of ident * arg list * statement * unit
//
// type stanProg = Prog of funDef list * statement * statement
//     | None
