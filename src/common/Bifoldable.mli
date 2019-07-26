module type Basic = sig
  type ('a, 'b) t [@@deriving fold]
end

module type S = sig
  type ('a, 'b) t

  val fold_left :
    f:('c -> 'a -> 'c) -> g:('c -> 'b -> 'c) -> init:'c -> ('a, 'b) t -> 'c

  val fold_right :
    f:('a -> 'c -> 'c) -> g:('b -> 'c -> 'c) -> init:'c -> ('a, 'b) t -> 'c

  val fold_map :
       (module Monoid.S with type t = 'a)
    -> f:('b -> 'a)
    -> g:('c -> 'a)
    -> ?init:'a
    -> ('b, 'c) t
    -> 'a

  val fold_left_first : f:('c -> 'a -> 'c) -> init:'c -> ('a, 'b) t -> 'c
  val fold_left_second : f:('c -> 'b -> 'c) -> init:'c -> ('a, 'b) t -> 'c
  val fold_right_first : f:('a -> 'c -> 'c) -> init:'c -> ('a, 'b) t -> 'c
  val fold_right_second : f:('b -> 'c -> 'c) -> init:'c -> ('a, 'b) t -> 'c

  val any :
       pred_first:('a -> bool)
    -> pred_second:('b -> bool)
    -> ?init:bool
    -> ('a, 'b) t
    -> bool

  val all :
       pred_first:('a -> bool)
    -> pred_second:('b -> bool)
    -> ?init:bool
    -> ('a, 'b) t
    -> bool
end

module Make (X : Basic) : S with type ('a, 'b) t := ('a, 'b) X.t
