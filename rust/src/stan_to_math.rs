use std::collections::HashMap;

lazy_static! {
    pub static ref STAN_TO_MATH: HashMap<&'static str, &'static str> = {
        let mut m = HashMap::new();
        m.insert("+", "add");
        m.insert("-", "subtract");
        m
    };
}

pub fn maybe_translate(s: String) -> String {
    match STAN_TO_MATH.get(s.as_ref() as &str) {
        Some(s) => s.to_string(),
        None => s
    }
}
