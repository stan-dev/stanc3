extern crate lalrpop;
use std::process::Command;

fn main() {
    lalrpop::process_root().unwrap();
    Command::new("bash")
        .arg("-c")
        .arg("cd math_iface; make math.a")
        .spawn()
        .expect("Failed to make math.a");
}
