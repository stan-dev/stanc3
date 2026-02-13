(** A non-empty list type. This can be constructed with the usual list syntax
    when the compiler is inferring the type, e.g.

    [let x : _ Nonempty_list.t = [1;2;3]]

    In Core v0.18, we can replace this with [Core.Nonempty_list] *)

type 'a t = ( :: ) of 'a * 'a list [@@deriving sexp]

val to_list : 'a t -> 'a list
val of_list_exn : 'a list -> 'a t
