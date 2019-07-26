module type Basic = sig
  type 'a t [@@deriving fold]
end

module type S = sig
  type 'a t

  val fold_right : f:('a -> 'b -> 'b) -> init:'b -> 'a t -> 'b
  val fold_left : f:('b -> 'a -> 'b) -> init:'b -> 'a t -> 'b

  val fold_map :
       (module Monoid.S with type t = 'a)
    -> f:('b -> 'a)
    -> ?init:'a
    -> 'b t
    -> 'a

  val any : pred:('a -> bool) -> ?init:bool -> 'a t -> bool
  val all : pred:('a -> bool) -> ?init:bool -> 'a t -> bool
  val to_list : 'a t -> 'a list
  val find : ('a -> bool) -> 'a t -> 'a option
end

module Make (X : Basic) : S with type 'a t := 'a X.t
