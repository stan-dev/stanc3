#[macro_use]
extern crate lalrpop_util;
#[macro_use]
extern crate lazy_static;
pub mod ast;
pub mod interpret;
pub mod optimize;
pub mod stan_to_math;

//use ast::Expr;
//use ast::Expr::{Int};
//
lalrpop_mod!(pub grammar); // synthesized by LALRPOP

fn main() {
    println!("Hello, world!");
}

#[test]
fn num_terms() {
    assert!(grammar::TermParser::new().parse("22").is_ok());
    assert!(grammar::TermParser::new().parse("(22)").is_ok());
    assert!(grammar::TermParser::new().parse("((((22))))").is_ok());
    assert!(grammar::TermParser::new().parse("((22)").is_err());
}

#[test]
fn exprs_test() {
    let ep = grammar::ExprParser::new();
    assert!(ep.parse("22 + 23").is_ok());
    assert!(ep.parse("(22) * 23 + 21").is_ok());
    assert!(ep.parse("((((22.1)))) / 2").is_ok());
    // match ep.parse("1 * 2 + 3") {
    //     //Ok(box Expr::BinOp(box Expr::BinOp(i, BinOp::Mul, j), BinOp::Add, k))
    //     //    => println!("{:?} {:?} {:?}", i, j, k),
    //     Ok(other) => panic!("Precedence not respected; {:?}", other),
    //     Err(x) => panic!("Failed to parse! {:?}", x),
    // }
    assert_eq!(interpret::eval_scal(&ep.parse("2 + 3 * 4").unwrap()), 14.0);
    assert_eq!(interpret::eval_scal(&ep.parse("2 * 4 - 1").unwrap()), 7.0);
    assert_eq!(interpret::eval_scal(&ep.parse("2 * (4 - 1)").unwrap()), 6.0);
    assert_eq!(
        interpret::eval_scal(&ep.parse("ident(10) + 1").unwrap()),
        11.0
    );
}

#[test]
fn optimize_test() {
    let sp = grammar::StatementsParser::new();

    let ast = sp.parse("ident(10) + ident(10)").unwrap();
    let optimized_ast = optimize::optimize(ast.clone());
    assert_eq!(interpret::eval_statements(&ast), 20.0);

    let mut optimized_interpreter = interpret::Interpreter::<interpret::StatsTracer>::new();
    assert_eq!(optimized_interpreter.eval_statements(&optimized_ast), 20.0);
    assert_eq!(optimized_interpreter.tracer.call_count("ident"), 1);
}

#[test]
fn ownership_vs_mutation() {
    let mut x = String::from("hi");
    mut_str(&mut x);
    mut_str(&mut x);
    println!("{}", &x);
    let s = String::from("hi");
    let s = rnstr(s);
    let s = rnstr(s);
    println!("{}", &s);
    assert_eq!(s, x);
}

fn mut_str(s: &mut String) {
    s.push_str(" world");
}

fn rnstr(mut s: String) -> String {
    s.push_str(" world");
    s
}
