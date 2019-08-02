val option_or_else : if_none:'a option -> 'a option -> 'a option
val on_fst : ('a -> 'c) -> 'a * 'b -> 'c * 'b
val on_snd : ('b -> 'c) -> 'a * 'b -> 'a * 'c
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
