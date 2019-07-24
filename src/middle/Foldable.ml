open Core_kernel

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

module Make (X : Basic) : S with type 'a t := 'a X.t = struct
  let fold_left ~f ~init x = X.fold f init x

  let fold_right ~f ~init x =
    let f' k x z = k @@ f x z in
    fold_left ~f:f' ~init:(fun x -> x) x init

  let fold_map (type a) (module M : Monoid.S with type t = a) ~f
      ?init:(empty = M.empty) x =
    fold_right ~f:(fun x accu -> M.combine accu @@ f x) ~init:empty x

  let to_list x = fold_right ~f:(fun x xs -> x :: xs) ~init:[] x
  let any ~pred ?init x = fold_map (module Monoid.Bool_or) ~f:pred ?init x
  let all ~pred ?init x = fold_map (module Monoid.Bool_and) ~f:pred ?init x

  let find pred x =
    fold_right
      ~f:(fun x accu ->
        Option.first_some accu @@ if pred x then Some x else None )
      ~init:None x
end
