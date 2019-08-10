val option_or_else : if_none:'a option -> 'a option -> 'a option
val on_fst : ('a -> 'c) -> 'a * 'b -> 'c * 'b
val on_snd : ('b -> 'c) -> 'a * 'b -> 'a * 'c
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val pp_builtin_syntax : string Fmt.t
val pp_keyword : string Fmt.t
val pp_angle_brackets : 'a Fmt.t -> 'a Fmt.t
val pp_brackets_postfix : 'a Fmt.t -> 'a Fmt.t
