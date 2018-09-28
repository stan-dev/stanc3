use ast::Expr;
use ast::Expr::*;

pub fn eval_scal(e: &Expr) -> f64 {
    match e {
        FnApp(fname, args) => {
            let eargs: Vec<f64> = args.into_iter()
                .map(|e| { eval_scal(e) })
                .collect();
            match fname.as_str() {
                "+" => eargs[0] + eargs[1],
                "-" => eargs[0] - eargs[1],
                "*" => eargs[0] * eargs[1],
                "/" => eargs[0] / eargs[1],
                _ => panic!("Operator {} not handled!", fname),
            }
        }
        Int(i) => f64::from(*i),
        _ => panic!("haha"),
    }
}
