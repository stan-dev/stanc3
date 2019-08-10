open Core_kernel

let option_or_else ~if_none x = Option.first_some x if_none
let on_snd f (x, y) = (x, f y)
let on_fst f (x, y) = (f x, y)
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y
let pp_builtin_syntax = Fmt.(string |> styled `Yellow)
let pp_keyword = Fmt.(string |> styled `Blue)
let pp_angle_brackets pp_v ppf v = Fmt.pf ppf "@[<1><%a>@]" pp_v v
let pp_brackets_postfix pp_e ppf = Fmt.pf ppf {|%a[]|} pp_e
