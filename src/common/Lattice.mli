open Core_kernel

(** Unbounde Join Semi-lattice over `property` 
*)
module Unbounded : sig
  module type S = sig
    type t
    type property

    include Partial_order.S with type t := property

    val join : property -> property -> property
  end
end

(** Join Semi-lattice over `property` with least-element (bottom) defined as a 
function of a type `t` on which properties are being calculated 
*)
module type S = sig
  include Unbounded.S

  val least_element_of : t -> property
end

module Make_powerset (X : sig
  type t

  module Property : sig
    include Comparable.S
  end
end) : S with type t = X.t and type property = X.Property.Set.t

module Make_dual_powerset (X : sig
  type t

  module Property : sig
    include Comparable.S
  end

  val greatest_element_of : t -> Property.Set.t
end) : S with type t = X.t and type property = X.Property.Set.t

module Make_from_unbounded (X : Unbounded.S) :
  S with type t = X.t and type property = X.property option
