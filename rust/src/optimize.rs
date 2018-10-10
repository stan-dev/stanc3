use ast::Expr;
use ast::Expr::FnApp;
use ast::Expr::Var;
use std::collections::HashMap;

struct Sym {}

pub fn optimize(e: Expr) -> Expr {
    e
}

struct Optimizer {
    // Function applications to Vars with a symbol that will have the result
    fn_cache: HashMap<Expr, Expr>,
}

impl Optimizer {
    fn cse(&mut self, e: Box<Expr>) -> Box<Expr> {
        match e {
            // Put all FnApps in the hashmap.
            // If at the end we have just one, leave it alone.
            // If we have two or more, replace with a local variable.
            // The interpreter will then be responsible for looking up the var.
            // This pass should write out the list of vars somewhere.
            ref f @ box FnApp(_, _) => {
                let var = self
                    .fn_cache
                    .entry((**f).clone())
                    .or_insert(Var(String::from("new symbol lol XXX")));
                Box::new(var.clone())
            }
            anything_else => anything_else,
        }
    }
}

