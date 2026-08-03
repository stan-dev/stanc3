type t = |

let sexp_of_t (nothing : t) = match nothing with _ -> . [@@coverage off]
let compare (n1 : t) (n2 : t) = match (n1, n2) with _ -> . [@@coverage off]
