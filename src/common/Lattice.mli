module type S = sig
  type t
  type property

  val leq : property -> property -> bool
  val lub : property -> property -> property
  val least_element_of : t -> property
  val extremal_value_of : t -> property
end
