open Core_kernel

let option_or_else ~if_none x = Option.first_some x if_none
let on_snd f (x, y) = (x, f y)
let on_fst f (x, y) = (f x, y)
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y
