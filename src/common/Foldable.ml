(* This module follows the Jane Street organisation and naming conventions for 
'standard interfaces' i.e. commonly used signatures; briefly:

- `Basic` defines the _minimal_ set of type and function signatures that a
user must provide
- `S` defines the _complete_ set of type and functions signatures that will be
exposed by any module supporting the  `standard interface` associated with `S`
- `Make` is a functor (i.e. a function from module to module) that takes the 
user supplied `Basic` module and returns a completed module with the signature
defined by `S`

Where a standard interface is defined for difference arities of 
type constructors, the convention is to append the arity to the names of the 
signatures and functors e.g. `Basic2`, `S2` and `Make2`
*)
open Core_kernel

(* The `Basic` definition for type constructors with a single type variable. 

This signature just says that you have some type `'a t` and a function `fold` 
with type: `fold: ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b`. 

This function can be derived using `ppx_deriving`.
*)
module type Basic = sig
  type 'a t [@@deriving fold]
end

(* The complete module signature for `Foldable` for types with a single 
type variable.
*)
module type S = sig
  (** A data structure which can be folded *)
  type 'a t

  val fold_left : f:('b -> 'a -> 'b) -> init:'b -> 'a t -> 'b
  (** Left associative fold of a data structure; this is the same as the 
  function derived from `[@@deriving fold]` but with labelled arguments *)

  val fold_right : f:('a -> 'b -> 'b) -> init:'b -> 'a t -> 'b
  (** Right associative fold of a data structure *)

  val any : pred:('a -> bool) -> ?init:bool -> 'a t -> bool
  (** Test whether any element of the data structure satisfies the supplied 
  predicate. The optional argument `init` specifies the starting value and 
  defaults to `false`. *)

  val all : pred:('a -> bool) -> ?init:bool -> 'a t -> bool
  (** Test whether all elements of the the data structure satify the supplied
  predicate. The optional argument `init` specifies the starting value and 
  defaults to `true`. *)
end

module Make (X : Basic) : S with type 'a t := 'a X.t = struct
  let fold_left ~f ~init x = X.fold f init x

  (** Right associative fold in terms of left associative fold *)
  let fold_right ~f ~init x =
    let f' k x z = k @@ f x z in
    fold_left ~f:f' ~init:(fun x -> x) x init

  let any ~pred ?(init = false) x =
    fold_right ~f:(fun x accu -> accu || pred x) ~init x

  let all ~pred ?(init = true) x =
    fold_right ~f:(fun x accu -> accu && pred x) ~init x
end

(* The `Basic` definition for type constructors with two type variables. *)
module type Basic2 = sig
  type ('a, 'b) t [@@deriving fold]
end

(* The complete module signature for `Foldable` for types with a single 
type variable.
*)
module type S2 = sig
  type ('a, 'b) t

  val fold_left :
    f:('c -> 'a -> 'c) -> g:('c -> 'b -> 'c) -> init:'c -> ('a, 'b) t -> 'c

  val fold_right :
    f:('a -> 'c -> 'c) -> g:('b -> 'c -> 'c) -> init:'c -> ('a, 'b) t -> 'c

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

module Make2 (X : Basic2) : S2 with type ('a, 'b) t := ('a, 'b) X.t = struct
  let fold_left ~f ~g ~init x = X.fold f g init x

  let fold_right ~f ~g ~init x =
    let f' k x z = k @@ f x z and g' k x z = k @@ g x z in
    fold_left ~f:f' ~g:g' ~init:Fn.id x init

  let any ~pred_first ~pred_second ?(init = false) x =
    fold_right
      ~f:(fun x accu -> accu || pred_first x)
      ~g:(fun x accu -> accu || pred_second x)
      ~init x

  let all ~pred_first ~pred_second ?(init = true) x =
    fold_right
      ~f:(fun x accu -> accu && pred_first x)
      ~g:(fun x accu -> accu && pred_second x)
      ~init x
end
