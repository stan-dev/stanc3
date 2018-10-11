use ast::{Expr, Expr::*, Statement, Statement::*};
use std::str::FromStr;

use std::collections::HashMap;

// XXX When to de-sugar?
// separate phase?

pub trait Tracer {
    fn new() -> Self;
    fn trace_fn_eval(&mut self, fname: &String, eargs: &Vec<f64>);
}

impl Tracer for () {
    fn new() -> Self {()}
    fn trace_fn_eval(&mut self, _fname: &String, _eargs: &Vec<f64>) {}
}

pub struct StatsTracer {
    eval_counts: HashMap<String,i32>
}

impl Tracer for StatsTracer {
    fn new() -> Self {
        StatsTracer{
            eval_counts: HashMap::new(),
        }
    }

    fn trace_fn_eval(&mut self, fname: &String, _eargs: &Vec<f64>) {
        *self.eval_counts.entry(fname.to_string()).or_insert(0) += 1;
    }
}

impl StatsTracer {
    pub fn call_count<'a>(&self, fname: &str) -> i32 {
        *self.eval_counts.get(fname).unwrap_or(&0)
    }
}

pub fn eval_scal(e: &Expr) -> f64 {
    Interpreter::<()>::new().eval_scal(e)
}

pub fn eval_statements(s: &Vec<Statement>) -> f64 {
    Interpreter::<()>::new().eval_statements(s)
}

pub struct Interpreter<T: Tracer> {
    pub tracer: T,
    environment: HashMap<String, f64>,

}

impl<T: Tracer> Interpreter<T> {
    pub fn new() -> Self {
        Interpreter{
            tracer: T::new(),
            environment: HashMap::new(),
        }
    }

    pub fn eval_statement(&mut self, statement: &Statement) -> Option<f64> {
        match statement {
            Assign(symbol, expr) => {
                let result = self.eval_scal(expr);
                self.environment.insert(symbol.clone(), result);
                None
            },
            Expr(e) => {
                Some(self.eval_scal(e))
            }
        }
    }

    pub fn eval_statements(&mut self, statements: &Vec<Statement>) -> f64 {
        statements.iter().filter_map(|s| self.eval_statement(s)).last().expect("No expressions in program")
    }

    pub fn eval_scal(&mut self, e: &Expr) -> f64 {
        match e {
            FnApp(fname, args) => {
                let eargs: Vec<f64> = args.into_iter()
                    .map(|e| { self.eval_scal(e) })
                    .collect();

                self.tracer.trace_fn_eval(fname, &eargs);
                
                match fname.as_str() {
                    "ident" => eargs[0],
                    "+" => eargs[0] + eargs[1],
                    _ => panic!("Operator {} not handled!", fname),
                }
            }
            Int(i) => f64::from(*i),
            Real(s) => f64::from_str(s).unwrap(),
            Var(symbol) => {
                *self.environment.get(symbol).expect(&format!("Var `{}` used before write", symbol))
            }

            expr => panic!(format!("No Evaluator defined for {:?}", expr)),
        }
    }
}
