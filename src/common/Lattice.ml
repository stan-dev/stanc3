open Core_kernel

(** Unbounde Join Semi-lattice over `property` 
*)
module Unbounded = struct
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
end) : S with type t = X.t and type property = X.Property.Set.t = struct
  type t = X.t
  type property = X.Property.Set.t

  let leq x y = X.Property.Set.is_subset x ~of_:y
  let least_element_of _ = X.Property.Set.empty
  let join x y = X.Property.Set.union x y
end

module Make_dual_powerset (X : sig
  type t

  module Property : sig
    include Comparable.S
  end

  val greatest_element_of : t -> Property.Set.t
end) : S with type t = X.t and type property = X.Property.Set.t = struct
  type t = X.t
  type property = X.Property.Set.t

  let leq x y = X.Property.Set.is_subset y ~of_:x
  let least_element_of t = X.greatest_element_of t
  let join x y = X.Property.Set.inter x y
end

module Make_from_unbounded (X : Unbounded.S) :
  S with type t = X.t and type property = X.property option = struct
  type t = X.t
  type property = X.property option

  let least_element_of _ = None

  let join x y =
    match (x, y) with
    | Some a, Some b -> Some (X.join a b)
    | Some _, None -> x
    | _, _ -> y

  let leq x y =
    match (x, y) with
    | Some a, Some b -> X.leq a b
    | Some _, _ -> false
    | _, _ -> true
end
