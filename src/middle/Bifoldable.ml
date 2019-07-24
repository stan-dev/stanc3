open Core_kernel

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

module Make (X : Basic) : S with type ('a, 'b) t := ('a, 'b) X.t = struct
  let fold_left ~f ~g ~init x = X.fold f g init x

  let fold_right ~f ~g ~init x =
    let f' k x z = k @@ f x z and g' k x z = k @@ g x z in
    fold_left ~f:f' ~g:g' ~init:(fun x -> x) x init

  let fold_map (type a) (module M : Monoid.S with type t = a) ~f ~g
      ?init:(empty = M.empty) x =
    fold_right
      ~f:(fun x accu -> M.combine accu @@ f x)
      ~g:(fun x accu -> M.combine accu @@ g x)
      ~init:empty x

  let fold_left_first ~f ~init x = fold_left ~f ~g:(fun accu _ -> accu) ~init x

  let fold_left_second ~f ~init x =
    fold_left ~f:(fun accu _ -> accu) ~g:f ~init x

  let fold_right_first ~f ~init x =
    fold_right ~f ~g:(fun _ accu -> accu) ~init x

  let fold_right_second ~f ~init x =
    fold_right ~f:(fun _ accu -> accu) ~g:f ~init x

  let any ~pred_first ~pred_second ?init x =
    fold_map (module Monoid.Bool_or) ~f:pred_first ~g:pred_second ?init x

  let all ~pred_first ~pred_second ?init x =
    fold_map (module Monoid.Bool_and) ~f:pred_first ~g:pred_second ?init x
end
