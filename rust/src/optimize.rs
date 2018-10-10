use ast::Expr;
use ast::Expr::FnApp;
use ast::Expr::Var;
use std::collections::HashMap;

struct Sym {}

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
            ref f @ box FnApp(_, _) => Box::new(if self.fn_cache.contains_key(&f) {
                self.fn_cache.get(&f).unwrap().clone()
            } else {
                let new_var = Var(String::from("new symbol lol XXX"));
                self.fn_cache.insert(*f.clone(), new_var.clone());
                new_var
            }),
            anything_else => anything_else,
        }
    }
}

pub fn optimize(_e: &Expr) {}
