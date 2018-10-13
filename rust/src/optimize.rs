use ast::Expr::FnApp;
use ast::Expr::Var;
use ast::{Expr, Statement};
use std::collections::HashMap;

struct Sym {}

trait ExprRewriter {
    fn rewrite(&mut self, Expr) -> Expr;
}

fn walk_expr(e: Expr, optimizer: &mut impl ExprRewriter) -> Expr {
    let inner = match e {
        FnApp(name, args) => FnApp(
            name,
            args.into_iter().map(|e| walk_expr(e, optimizer)).collect(),
        ),
        otherwise => otherwise,
    };
    optimizer.rewrite(inner)
}

fn walk_statements(
    statements: Vec<Statement>,
    optimizer: &mut impl ExprRewriter,
) -> Vec<Statement> {
    statements
        .into_iter()
        .map(|s| match s {
            Statement::Assign(symbol, expr) => {
                Statement::Assign(symbol, walk_expr(expr, optimizer))
            }
            Statement::Expr(expr) => Statement::Expr(walk_expr(expr, optimizer)),
        })
        .collect()
}

pub fn optimize(statements: Vec<Statement>) -> Vec<Statement> {
    let mut const_fn_walker = ConstFnEvalOptimizer::default();
    let new_ast = walk_statements(statements, &mut const_fn_walker);
    const_fn_walker
        .fn_cache
        .into_iter()
        .map(|(expr, var)| Statement::Assign(var, expr))
        .chain(new_ast.into_iter())
        .collect()
}

#[derive(Default)]
struct ConstFnEvalOptimizer {
    // Function applications to Vars with a symbol that will have the result
    fn_cache: HashMap<Expr, String>,
}

impl ExprRewriter for ConstFnEvalOptimizer {
    fn rewrite(&mut self, e: Expr) -> Expr {
        match e {
            // Put all FnApps in the hashmap.
            // If at the end we have just one, leave it alone.
            // If we have two or more, replace with a local variable.
            // The interpreter will then be responsible for looking up the var.
            // This pass should write out the list of vars somewhere.
            FnApp(name, args) => {
                let args_are_const = args.iter().all(|a| match a {
                    Expr::Int(_) => true,
                    Expr::Real(_) => true,
                    _ => false,
                });

                if args_are_const {
                    let symbol = self
                        .fn_cache
                        .entry(FnApp(name, args))
                        .or_insert(String::from("new symbol lol XXX"));
                    Var(symbol.clone())
                } else {
                    FnApp(name, args)
                }
            }
            anything_else => anything_else,
        }
    }
}
