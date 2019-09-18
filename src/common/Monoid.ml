open Core_kernel
(** A `Monoid` is a standard interface for some type `t` which exposes a value
`empty` and a function `combine` such that:

`combine x empty === x === combine empty x`

for all values of x.
*)
module type S = sig
  type t

  val empty : t
  val combine : t -> t -> t
end

(** Make the dual of a monoid, itself a monoid, by reversing the arguments to 
the `combine` function. *)
module Make_dual(X : S) : S with type t = X.t = struct 
    type t = X.t 
    let empty = X.empty

    let combine a b = X.combine b a
end


(** The monoid for `bool` taking the associative `combine` function to be `or` 
and the `empty` value to be `false`: 

`x || false === x === false || x`
*)
module Bool_or : S with type t = bool = struct
  type t = bool

  let empty = false
  let combine a b = a || b
end

(** The monoid for `bool` taking the associative `combine` function to be `and` 
and the `empty` value to be `true`: 

`x && true === x === true && x`
*)
module Bool_and : S with type t = bool = struct
  type t = bool

  let empty = true
  let combine a b = a && b
end

(** Make the monoid for `t list` constructed from a module defining `t` taking
the `combine` function to be `List.append` and the `empty` to be the empty list.

`x @ [] = x = [] @ x`
*)
module Make_list(X: sig type t end) : S with type t = X.t list = struct
    type t = X.t list 
    let empty = [] 
    let combine a b  = a @ b
    
end

(** Make the monoid for `t option` constructed from a module defining `t` taking
the `empty` value to be `None` and `combine` equal to `Option.first_some`.

`x @ None = x = None @ x`
*)
module Make_option_first (X : sig
  type t
end) : S with type t = X.t option = struct
  type t = X.t option

  let empty = None
  let combine a b =  
    Option.first_some a b
    
end

(** Make the monoid for `t option` constructed from a module defining `t` taking
the `empty` value to be `None` and `combine` equal to `Option.first_some` with 
arguments reversed.

`x @ None = x = None @ x`
*)
module Make_option_last (X : sig
  type t
end) : S with type t = X.t option = Make_dual(Make_option_first(X))