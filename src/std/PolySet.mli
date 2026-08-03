(** Polymorphic sets. This is a copy of Stdlib.Set.Make with [type elt] replaced
    by a ['a], and [Ord.compare] replaced with [Stdlib.compare ]*)

type 'a t

val empty : 'a t
val add : 'a -> 'a t -> 'a t
val singleton : 'a -> 'a t
val remove : 'a -> 'a t -> 'a t
val union : 'a t -> 'a t -> 'a t
val inter : 'a t -> 'a t -> 'a t
val disjoint : 'a t -> 'a t -> bool
val diff : 'a t -> 'a t -> 'a t
val cardinal : 'a t -> int
val elements : 'a t -> 'a list
val min_elt : 'a t -> 'a
val min_elt_opt : 'a t -> 'a option
val max_elt : 'a t -> 'a
val max_elt_opt : 'a t -> 'a option
val choose : 'a t -> 'a
val choose_opt : 'a t -> 'a option
val find_first : ('a -> bool) -> 'a t -> 'a
val find_first_opt : ('a -> bool) -> 'a t -> 'a option
val find_last : ('a -> bool) -> 'a t -> 'a
val find_last_opt : ('a -> bool) -> 'a t -> 'a option
val iter : f:('a -> unit) -> 'a t -> unit
val fold : f:('a -> 'b -> 'b) -> init:'b -> 'a t -> 'b
val map : f:('a -> 'b) -> 'a t -> 'b t
val filter : f:('a -> bool) -> 'a t -> 'a t
val filter_map : f:('a -> 'b option) -> 'a t -> 'b t
val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
val split : 'a -> 'a t -> 'a t * bool * 'a t
val is_empty : 'a t -> bool
val is_singleton : 'a t -> bool
val mem : 'a -> 'a t -> bool
val equal : 'a t -> 'a t -> bool
val compare : 'a t -> 'a t -> int
val subset : 'a t -> 'a t -> bool
val for_all : f:('a -> bool) -> 'a t -> bool
val exists : f:('a -> bool) -> 'a t -> bool
val to_list : 'a t -> 'a list
val of_list : 'a list -> 'a t
val to_seq_from : 'a -> 'a t -> 'a Seq.t
val to_seq : 'a t -> 'a Seq.t
val to_rev_seq : 'a t -> 'a Seq.t
val add_seq : 'a Seq.t -> 'a t -> 'a t
val of_seq : 'a Seq.t -> 'a t

(** Added *)

val union_map : f:('a -> 'b t) -> 'a t -> 'b t
val union_list : 'a t list -> 'a t
val sexp_of_t : ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t
