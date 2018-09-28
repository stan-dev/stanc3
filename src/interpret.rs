
use ast::Expr;
use ast::Expr::*;
use ast::BinOp::*;

pub fn evalScal(e: &Expr) -> f64 {
    match e {
        BinOp(ref lu, op, ref ru) => {
            let l = evalScal(lu);
            let r = evalScal(ru);
            match op {
                Add => l + r,
                Sub => l - r,
                Div => l / r,
                Mul => l * r,
                _ => panic!("need more binary operators implemented"),
            }
        }
        Int(i) => f64::from(*i),
        _ => panic!("haha"),
    }
}
