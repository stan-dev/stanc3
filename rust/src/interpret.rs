use ast::Expr;
use ast::Expr::*;
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

pub struct Interpreter<T: Tracer> {
    pub tracer: T
}

impl<T: Tracer> Interpreter<T> {
    pub fn new() -> Self {
        Interpreter{
            tracer: T::new(),
        }
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
            _ => panic!("haha"),
        }
    }
}
