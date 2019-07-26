open Core_kernel

let option_or_else ~if_none x = Option.first_some x if_none
