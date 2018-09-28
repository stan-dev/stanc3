#[derive(Debug)]
pub enum Expr {
    Int(i32),
    Str(String),
    Real(f64),
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    UnaryOp(UnaryOp, Box<Expr>),
    Var(String),
    FnApp(String, Vec<Expr>),
}

#[derive(Debug)]
pub enum UnaryOp {Not, Neg, Transpose}

#[derive(Debug)]
pub enum BinOp {
    Mul, Div, Add, Sub, Mod, CoeffMul, And, Or, Lte, Gte, Neq, Eq, Lt, Gt
}

pub enum TypePrim {
    Bool, Real, Int, Array(Box<TypePrim>, Vec<Expr>),
    Vector(Box<Expr>), Matrix(Box<Expr>, Box<Expr>), Unit
}

pub enum TypeLevel {
    LevelVar(String), Data, Model, GenQuant, Lub(Vec<TypeLevel>), Glb(Vec<TypeLevel>)
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
