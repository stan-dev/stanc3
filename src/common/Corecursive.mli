module Fn = Core_kernel.Fn
module Either = Core_kernel.Either
module Tuple = Core_kernel.Tuple

module type Basic = sig
  module Pattern : Functor.S

  type t

  val inj : t Pattern.t -> t
  val proj : t -> t Pattern.t
end

module type S = sig
  module Pattern : Functor.S

  type t
  type 'a coalgebra = 'a -> 'a Pattern.t

  val ana : ('a -> 'a Pattern.t) -> 'a -> t
  val transform_top_down : (t -> t) -> t -> t

  type 'a r_coalgebra = 'a -> (t, 'a) Either.t Pattern.t

  val apo : ('a -> (t, 'a) Either.t Pattern.t) -> 'a -> t
  val transform_partial : (t -> (t, t) Either.t) -> t -> t
end

module Make (X : Basic) : S with type t := X.t and module Pattern := X.Pattern

module type Basic1 = sig
  module Pattern : Functor.S

  type 'a t

  val inj : 'a * 'a t Pattern.t -> 'a t
  val proj : 'a t -> 'a * 'a t Pattern.t
end

module type S1 = sig
  module Pattern : Functor.S

  type 'a t
  type ('a, 'r) coalgebra = 'r -> 'a * 'r Pattern.t

  val ana : ('a, 'r) coalgebra -> 'r -> 'a t
  val transform_top_down : ('a t -> 'a t) -> 'a t -> 'a t

  type ('a, 'r) r_coalgebra = 'r -> 'a * ('a t, 'r) Either.t Pattern.t

  val apo : ('a, 'r) r_coalgebra -> 'r -> 'a t
  val transform_partial : ('a t -> ('a t, 'a t) Either.t) -> 'a t -> 'a t
end

module Make1 (X : Basic1) :
  S1 with type 'a t := 'a X.t and module Pattern := X.Pattern

module type Basic2 = sig
  type ('a, 'b) t

  module Pattern : Bifunctor.S

  module First : sig
    include Basic1
    include S1 with type 'a t := 'a t and module Pattern := Pattern
  end

  val inj : 'b * ('a First.t, ('a, 'b) t) Pattern.t -> ('a, 'b) t
  val proj : ('a, 'b) t -> 'b * ('a First.t, ('a, 'b) t) Pattern.t
end

module type S2 = sig
  type ('a, 'b) t

  module First : S1
  module Pattern : Bifunctor.S

  type ('a, 'b, 'r) coalgebra = 'r -> 'b * ('a, 'r) Pattern.t

  val ana :
    ('a, 'r1) First.coalgebra -> ('r1, 'b, 'r2) coalgebra -> 'r2 -> ('a, 'b) t

  val transform_top_down :
       ('a First.t -> 'a First.t)
    -> (('a, 'b) t -> ('a, 'b) t)
    -> ('a, 'b) t
    -> ('a, 'b) t

  type ('a, 'b, 'r1, 'r2) r_coalgebra =
       'r2
    -> 'b * (('a First.t, 'r1) Either.t, (('a, 'b) t, 'r2) Either.t) Pattern.t

  val apo :
       ('a, 'r1) First.r_coalgebra
    -> ('a, 'b, 'r1, 'r2) r_coalgebra
    -> 'r2
    -> ('a, 'b) t

  val transform_partial :
       ('a First.t -> ('a First.t, 'a First.t) Either.t)
    -> (('a, 'b) t -> (('a, 'b) t, ('a, 'b) t) Either.t)
    -> ('a, 'b) t
    -> ('a, 'b) t
end

module Make2 (X : Basic2) :
  S2
  with type ('a, 'b) t := ('a, 'b) X.t
   and module Pattern := X.Pattern
   and module First := X.First
