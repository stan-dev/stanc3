(** A non-empty list type. This can be constructed with the usual list syntax
    when the compiler is inferring the type, e.g.

    [let x : _ Nonempty_list.t = [1;2;3]] *)

type 'a t = ( :: ) of 'a * 'a list

val of_list : 'a list -> 'a t option
val of_list_exn : 'a list -> 'a t
