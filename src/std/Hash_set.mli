(** Hash sets are implemented as a hashmap from ['a] to [unit] *)

type 'a t

val create : int -> 'a t
val mem : 'a t -> 'a -> bool
val add : 'a t -> 'a -> unit
val iter : f:('a -> unit) -> 'a t -> unit
val length : 'a t -> int
val is_empty : 'a t -> bool
val union : 'a t -> 'a t -> 'a t
