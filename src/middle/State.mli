open Core_kernel

module type S = sig
  include Applicative.S2
  include Monad.S2 with type ('a, 'state) t := ('a, 'state) t

  val get : ('state, 'state) t
  val put : 'state -> (unit, 'state) t
  val modify : ('state -> 'state) -> (unit, 'state) t
  val with_state : ('a, 'state) t -> f:('state -> 'state) -> ('a, 'state) t
  val run_state : ('a, 'state) t -> init:'state -> 'a * 'state
end

module State : S

module Cps : sig
  module State : S
end
