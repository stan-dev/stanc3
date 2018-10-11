#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

extern "C" {
    #[link_name = "lognormal_ccdf_log"]
    pub fn lognormal_ccdf_log(a: f64, b: f64, c: f64) -> f64;
}

fn main() {
    println!("Hello, world!");
    unsafe {
        println!("{}", lognormal_ccdf_log(1f64,1f64,1f64));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lognormal_ccdf_log() {
        unsafe {
            assert_eq!(lognormal_ccdf_log(0f64,0f64,0f64), 0f64);
        }
    }
}
