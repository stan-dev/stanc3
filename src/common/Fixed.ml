(** This module defines the signatures and [Make] functors for the 'fixed point'
    (or two-level) type we use for our intermediate representations
*)

(** A [Pattern] defines the signature of modules that may be fixed with
[Fixed.Make] and [Fixed.Make2].

These signatures ensure that all the operations we want to support on our
top level intermediate representations can be defined by the [Fixed.Make]
functors.
*)
module Patterns = struct
  module type S = sig
    type 'a t [@@deriving compare, fold, hash, map, sexp]

    val pp : 'a Fmt.t -> 'a t Fmt.t
  end

  module type S2 = sig
    type ('a, 'b) t [@@deriving compare, fold, hash, map, sexp]

    val pp : 'a Fmt.t -> 'b Fmt.t -> ('a, 'b) t Fmt.t
  end
end

(** The fixed-point of [Pattern.t] annotated with some meta-data *)
module type S = sig
  module Pattern : Patterns.S

  type 'a t = {pattern: 'a t Pattern.t; meta: 'a}
  [@@deriving compare, hash, sexp]

  val pp : 'a t Fmt.t

  val rewrite_bottom_up : f:('a t -> 'a t) -> 'a t -> 'a t
  (** [rewrite_bottom_up] specializes [fold] so that the result type
  ['r] is equal to the type of our fixed-point data structure i.e. ['r = 'a t].
  This also means that the function [f] can be written with our fixed-point type
  ['a t] as its argument. *)
end

(** Functor  which creates the fixed-point of the type defined in the [Pattern]
module argument
*)
module Make (Pattern : Patterns.S) : S with module Pattern := Pattern = struct
  type 'a t = {pattern: 'a t Pattern.t; meta: 'a}
  [@@deriving compare, hash, sexp]

  let rec pp ppf {pattern; _} = (Pattern.pp pp) ppf pattern

  (** For clarity this is written explicitly but is equivalent to
  {[fold_pattern ~f:(Fn.compose f fix) t]}
  *)
  let rec rewrite_bottom_up ~f t =
    let x = {t with pattern= Pattern.map (rewrite_bottom_up ~f) t.pattern} in
    f x
end

(** Nested fixed-point type where an element of the [Pattern] is itself
a fixed-point type. We use this to represent statements which contain
expressions.
*)
module type S2 = sig
  module First : S
  module Pattern : Patterns.S2

  type ('a, 'b) t = {pattern: ('a First.t, ('a, 'b) t) Pattern.t; meta: 'b}
  [@@deriving compare, hash, sexp]

  val pp : ('a, 'b) t Fmt.t

  val rewrite_bottom_up :
       f:('a First.t -> 'a First.t)
    -> g:(('a, 'b) t -> ('a, 'b) t)
    -> ('a, 'b) t
    -> ('a, 'b) t
  (** [rewrite_bottom_up] specializes [fold] so that the result type
  ['r1] is equal to the type of the nested fixed-point type
  i.e. ['r1 = 'a First.t] and the result type ['r2] is equal to the top-level
  fixed-point type i.e. ['r2 = ('a,'b) t].

  This also means that the function [f] can be written with our nested
  fixed-point type  ['a First.t] as its argument and [g] can be written with
  [('a,'b) t] as its argument.
  *)
end

module Make2 (First : S) (Pattern : Patterns.S2) :
  S2 with module First := First and module Pattern := Pattern = struct
  type ('a, 'b) t = {pattern: ('a First.t, ('a, 'b) t) Pattern.t; meta: 'b}
  [@@deriving compare, hash, sexp]

  let rec pp ppf {pattern; _} = (Pattern.pp First.pp pp) ppf pattern

  let rec rewrite_bottom_up ~f ~g t =
    g
      { t with
        pattern=
          Pattern.map
            (First.rewrite_bottom_up ~f)
            (rewrite_bottom_up ~f ~g) t.pattern }
end
