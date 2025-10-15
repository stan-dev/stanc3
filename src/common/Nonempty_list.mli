(** A non-empty list type. This can be constructed with the usual
list syntax when the compiler is inferring the type, e.g.

[let x : _ Nonempty_list.t = [1;2;3]]

In Core v0.18, we can replace this with [Core.Nonempty_list]
*)

type 'a t = ( :: ) of 'a * 'a list
[@@deriving compare, equal, hash, map, fold, sexp]

val to_list : 'a t -> 'a list
val of_list_exn : 'a list -> 'a t

val fold_map : 'a t -> init:'b -> f:('b -> 'a -> 'b * 'c) -> 'b * 'c t
(** Wrapper around [Core.List.fold_map] *)
