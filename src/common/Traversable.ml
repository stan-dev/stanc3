open Core_kernel

module type S = sig
  type 'a t

  module A : Applicative.S

  val traverse : 'a t -> f:('a -> 'b A.t) -> 'b t A.t
end

module type S2 = sig
  type 'a t

  module A : Applicative.S2

  val traverse : 'a t -> f:('a -> ('b, 'e) A.t) -> ('b t, 'e) A.t
end
