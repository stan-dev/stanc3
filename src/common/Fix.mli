open Core_kernel

module type S = sig
  module Pattern : Pattern.S

  type 'a t [@@deriving compare, map, fold, hash, sexp]

  (* TODO : derive *)
  module Make_traversable (A : Applicative.S) :
    Traversable.S with module A := A and type 'a t := 'a t

  module Make_traversable2 (A : Applicative.S2) :
    Traversable.S2 with module A := A and type 'a t := 'a t

  include Foldable.S with type 'a t := 'a t
  include Pretty.S1 with type 'a t := 'a t
  include Recursive.S1 with type 'a t := 'a t and module Pattern := Pattern

  val proj : 'a t -> 'a * 'a t Pattern.t
  val pattern : 'a t -> 'a t Pattern.t
  val meta : 'a t -> 'a
  val inj : 'a * 'a t Pattern.t -> 'a t
  val fix : 'a -> 'a t Pattern.t -> 'a t
end

module type S2 = sig
  module First : S
  module Pattern : Pattern.S2

  type ('a, 'b) t [@@deriving compare, map, fold, hash, sexp]

  (* TODO : derive *)
  module Make_traversable (A : Applicative.S) :
    Bitraversable.S with module A := A and type ('a, 'b) t := ('a, 'b) t

  module Make_traversable2 (A : Applicative.S2) :
    Bitraversable.S2 with module A := A and type ('a, 'b) t := ('a, 'b) t

  include Bifoldable.S with type ('a, 'b) t := ('a, 'b) t
  include Pretty.S2 with type ('a, 'b) t := ('a, 'b) t

  include
    Recursive.S2
    with type ('a, 'b) t := ('a, 'b) t
     and module First := First
     and module Pattern := Pattern

  val proj : ('a, 'b) t -> 'b * ('a First.t, ('a, 'b) t) Pattern.t
  val pattern : ('a, 'b) t -> ('a First.t, ('a, 'b) t) Pattern.t
  val meta : ('a, 'b) t -> 'b
  val inj : 'b * ('a First.t, ('a, 'b) t) Pattern.t -> ('a, 'b) t
  val fix : 'b -> ('a First.t, ('a, 'b) t) Pattern.t -> ('a, 'b) t
end

module Make (Pattern : Pattern.S) : S with module Pattern := Pattern

module Make2 (First : S) (Pattern : Pattern.S2) :
  S2 with module First := First and module Pattern := Pattern
